use std::{any::Any, io, path::PathBuf, sync::Arc, time::SystemTime};

use crate::*;

use ::tracing;
use serde_with::{DisplayFromStr, FromInto};
use tracing_subscriber::Layer as _;

serde! {
    pub enum AnyLayer {
        Fmt(FmtLayer),
        Journald(JournaldLayer),
        Flame(FlameLayer),
        Chrome(ChromeLayer),
        Tree(HierarchicalLayer),
    }
}

impl AnyLayer {
    pub fn build<S>(
        self,
    ) -> Result<
        (
            Box<dyn tracing_subscriber::Layer<S> + Send + Sync>,
            DynGuard,
        ),
        BoxError,
    >
    where
        S: tracing::Subscriber + for<'a> tracing_subscriber::registry::LookupSpan<'a>,
        S: Send + Sync,
    {
        Ok(match self {
            AnyLayer::Fmt(it) => {
                let (lyr, gd) = it.build()?;
                (lyr.boxed(), DynGuard::new(gd))
            }
            AnyLayer::Journald(it) => (it.build()?.boxed(), DynGuard::new(())),
            AnyLayer::Flame(it) => (it.build()?.boxed(), DynGuard::new(())),
            AnyLayer::Chrome(it) => {
                let (lyr, gd) = it.builder()?;
                (lyr.boxed(), DynGuard::new(gd))
            }
            AnyLayer::Tree(it) => (it.build()?.boxed(), DynGuard::new(())),
        })
    }
}

serde! {
    #[derive(Default)]
    pub struct FmtLayer {
        pub format: Option<FmtLayerFormat>,
        pub writer: Option<Writer>,
        pub non_blocking: Option<NonBlocking>,
        pub filter: Option<AnyFilter>,
    }

    #[derive(Default)]
    pub struct FmtLayerFormat {
        pub formatter: Option<Formatter>,
        pub options: Option<FormatOptions>,
        pub span_events: Option<FmtSpan>,
        pub timer: Option<FormatTime>,
    }

    #[serde(untagged)]
    pub enum AnyFilter {
        Level {
            #[serde_as(as = "FromInto<_LevelFilter>")]
            level: tracing::metadata::LevelFilter,
        },
        Targets(Targets),
        EnvFilterDirectives(EnvFilterDirectives),
        EnvFilterFromEnv(EnvFilterFromEnv),
    }
}

impl FmtLayer {
    #[expect(clippy::type_complexity)]
    pub fn build<S>(
        self,
    ) -> Result<
        (
            tracing_subscriber::filter::Filtered<
                Box<dyn tracing_subscriber::Layer<S> + Send + Sync>,
                Box<dyn tracing_subscriber::layer::Filter<S> + Send + Sync>,
                S,
            >,
            Option<tracing_appender::non_blocking::WorkerGuard>,
        ),
        BoxError,
    >
    where
        S: tracing::Subscriber + for<'a> tracing_subscriber::registry::LookupSpan<'a>,
    {
        let Self {
            format,
            writer,
            non_blocking,
            filter,
        } = self;
        let writer = writer.unwrap_or(Writer::Stdout);
        let (mw, gd) = MaybeNonBlockingWriter {
            writer,
            non_blocking,
        }
        .build_make_writer()?;
        let lyr = format
            .unwrap_or_default()
            .apply(tracing_subscriber::fmt::layer().with_writer(mw))
            .with_filter(
                filter
                    .unwrap_or(AnyFilter::Level {
                        level: tracing::metadata::LevelFilter::WARN,
                    })
                    .build(),
            );
        Ok((lyr, gd))
    }
}

impl FmtLayerFormat {
    // the fmt_fields needs to match the evt_format, else we hit a `malformed fields` panic in the JSON formatter:
    // https://github.com/tokio-rs/tracing/blob/cc44064b3a41cb586bd633f8a024354928e25819/tracing-subscriber/src/fmt/format/json.rs#L184
    //
    // so this defines our type erasure boundary.
    pub fn apply<S, N, E, W>(
        self,
        to: tracing_subscriber::fmt::Layer<S, N, E, W>,
    ) -> Box<dyn tracing_subscriber::Layer<S> + Send + Sync>
    where
        S: tracing::Subscriber + for<'a> tracing_subscriber::registry::LookupSpan<'a>,
        W: for<'writer> tracing_subscriber::fmt::MakeWriter<'writer> + 'static + Send + Sync,
    {
        let Self {
            formatter,
            options,
            span_events,
            timer,
        } = self;
        let options = options.unwrap_or_default();
        let span_events = span_events.unwrap_or_default().build();
        let timer = timer.unwrap_or_default().build();
        match formatter.unwrap_or_default() {
            Formatter::Full => to
                .fmt_fields(tracing_subscriber::fmt::format::DefaultFields::new())
                .event_format(options.apply(tracing_subscriber::fmt::format::Format::default()))
                .with_timer(timer)
                .with_span_events(span_events)
                .boxed(),
            Formatter::Compact => to
                .fmt_fields(tracing_subscriber::fmt::format::DefaultFields::new())
                .event_format(
                    options.apply(tracing_subscriber::fmt::format::Format::default().compact()),
                )
                .with_timer(timer)
                .with_span_events(span_events)
                .boxed(),
            Formatter::Pretty => to
                .fmt_fields(tracing_subscriber::fmt::format::PrettyFields::new())
                .event_format(
                    options.apply(tracing_subscriber::fmt::format::Format::default().pretty()),
                )
                .with_timer(timer)
                .with_span_events(span_events)
                .boxed(),
            Formatter::Json(json) => {
                to.fmt_fields(tracing_subscriber::fmt::format::JsonFields::new())
                    .event_format(options.apply(
                        json.apply(tracing_subscriber::fmt::format::Format::default().json()),
                    ))
                    .with_timer(timer)
                    .with_span_events(span_events)
                    .boxed()
            }
        }
    }
}

impl AnyFilter {
    pub fn build<S: tracing::Subscriber>(
        self,
    ) -> Box<dyn tracing_subscriber::layer::Filter<S> + Send + Sync> {
        match self {
            AnyFilter::Level { level } => Box::new(level),
            AnyFilter::Targets(it) => Box::new(it.build()),
            AnyFilter::EnvFilterDirectives(it) => Box::new(it.build()),
            AnyFilter::EnvFilterFromEnv(it) => Box::new(it.build()),
        }
    }
}

// cannot be Send + Sync because of ChromeLayer guard
#[must_use]
#[expect(dead_code)]
pub struct DynGuard(Box<dyn Any>);

impl DynGuard {
    pub fn new<T: 'static>(t: T) -> Self {
        Self(Box::new(t))
    }
}

serde! {
    #[derive(Default)]
    pub struct FormatOptions {
        pub ansi: Option<bool>,
        pub target: Option<bool>,
        pub level: Option<bool>,
        pub thread_ids: Option<bool>,
        pub thread_names: Option<bool>,
        pub file: Option<bool>,
        pub line_number: Option<bool>,
    }

    #[derive(Default)]
    pub struct FormatJsonOptions {
        pub flatten_event: Option<bool>,
        pub with_current_span: Option<bool>,
        pub with_span_list: Option<bool>,
    }

    #[derive(Default)]
    pub enum Formatter {
        #[default]
        Full,
        Compact,
        Pretty,
        Json(FormatJsonOptions)
    }

    #[derive(Default)]
    pub enum FormatTime {
        None,
        ChronoLocal {
            format: Option<String>,
        },
        ChronoUtc {
            format: Option<String>,
        },
        #[default]
        SystemTime,
        Uptime,
    }

    #[derive(Default)]
    pub struct FmtSpan {
        pub new: Option<bool>,
        pub enter: Option<bool>,
        pub exit: Option<bool>,
        pub close: Option<bool>,
    }
}

impl FmtSpan {
    #[expect(clippy::let_and_return)]
    pub fn build(self) -> tracing_subscriber::fmt::format::FmtSpan {
        use tracing_subscriber::fmt::format::FmtSpan;
        let Self {
            new,
            enter,
            exit,
            close,
        } = self;
        fn apply(fmt: FmtSpan, cond: Option<bool>, val: FmtSpan) -> FmtSpan {
            match cond {
                Some(true) => fmt | val,
                Some(false) | None => fmt,
            }
        }
        let v = FmtSpan::NONE;
        let v = apply(v, new, FmtSpan::NEW);
        let v = apply(v, enter, FmtSpan::ENTER);
        let v = apply(v, exit, FmtSpan::EXIT);
        let v = apply(v, close, FmtSpan::CLOSE);
        v
    }
}

pub struct DynFormatTime(Arc<dyn tracing_subscriber::fmt::time::FormatTime + Send + Sync>);

impl DynFormatTime {
    pub fn new<F: tracing_subscriber::fmt::time::FormatTime + Send + Sync + 'static>(f: F) -> Self {
        Self(Arc::new(f))
    }
}
impl tracing_subscriber::fmt::time::FormatTime for DynFormatTime {
    fn format_time(&self, w: &mut tracing_subscriber::fmt::format::Writer<'_>) -> fmt::Result {
        self.0.format_time(w)
    }
}

impl FormatTime {
    pub fn build(self) -> DynFormatTime {
        match self {
            FormatTime::None => DynFormatTime::new(()),
            FormatTime::ChronoLocal { format: None } => {
                DynFormatTime::new(tracing_subscriber::fmt::time::ChronoLocal::rfc_3339())
            }
            FormatTime::ChronoLocal { format: Some(fmt) } => {
                DynFormatTime::new(tracing_subscriber::fmt::time::ChronoLocal::new(fmt))
            }
            FormatTime::ChronoUtc { format: None } => {
                DynFormatTime::new(tracing_subscriber::fmt::time::ChronoUtc::rfc_3339())
            }
            FormatTime::ChronoUtc { format: Some(fmt) } => {
                DynFormatTime::new(tracing_subscriber::fmt::time::ChronoUtc::new(fmt))
            }
            FormatTime::SystemTime => DynFormatTime::new(tracing_subscriber::fmt::time::SystemTime),
            FormatTime::Uptime => {
                DynFormatTime::new(tracing_subscriber::fmt::time::Uptime::from(Instant::now()))
            }
        }
    }
}

impl FormatOptions {
    pub fn apply<F, T>(
        self,
        to: tracing_subscriber::fmt::format::Format<F, T>,
    ) -> tracing_subscriber::fmt::format::Format<F, T> {
        let Self {
            ansi,
            target,
            level,
            thread_ids,
            thread_names,
            file,
            line_number,
        } = self;
        to.tap_opt(ansi, |b, v| b.with_ansi(v))
            .tap_opt(target, |b, v| b.with_target(v))
            .tap_opt(level, |b, v| b.with_level(v))
            .tap_opt(thread_ids, |b, v| b.with_thread_ids(v))
            .tap_opt(thread_names, |b, v| b.with_thread_names(v))
            .tap_opt(file, |b, v| b.with_file(v))
            .tap_opt(line_number, |b, v| b.with_line_number(v))
    }
}

impl FormatJsonOptions {
    pub fn apply<T>(
        self,
        to: tracing_subscriber::fmt::format::Format<tracing_subscriber::fmt::format::Json, T>,
    ) -> tracing_subscriber::fmt::format::Format<tracing_subscriber::fmt::format::Json, T> {
        let Self {
            flatten_event,
            with_current_span,
            with_span_list,
        } = self;
        to.tap_opt(flatten_event, |b, v| b.flatten_event(v))
            .tap_opt(with_current_span, |b, v| b.with_current_span(v))
            .tap_opt(with_span_list, |b, v| b.with_span_list(v))
    }
}

serde! {
    #[derive(Default)]
    pub struct NonBlocking {
        pub buffered_lines_limit: Option<usize>,
        pub lossy: Option<bool>,
        pub thread_name: Option<String>,
    }

    #[derive(Default)]
    pub struct Rolling {
        #[serde_as(as = "Option<FromInto<_Rotation>>")]
        pub rotation: Option<tracing_appender::rolling::Rotation>,
        pub filename_prefix: Option<String>,
        pub filename_suffix: Option<String>,
        pub max_log_files: Option<usize>,
    }

    enum _Rotation {
        Minutely,
        Hourly,
        Daily,
        Weekly,
        Never,
    }

    pub enum Writer {
        File {
            path: PathBuf,
            options: OpenOptions,
        },
        Stdout,
        Stderr,
        TestStdout,
        TestStderr,
        Rolling {
            directory: PathBuf,
            config: Option<Rolling>,
        }
    }

    pub struct MaybeNonBlockingWriter {
        pub writer: Writer,
        pub non_blocking: Option<NonBlocking>,
    }
}

impl MaybeNonBlockingWriter {
    pub fn build_make_writer(
        self,
    ) -> Result<
        (
            tracing_subscriber::fmt::writer::BoxMakeWriter,
            Option<tracing_appender::non_blocking::WorkerGuard>,
        ),
        BoxError,
    > {
        let Self {
            writer,
            non_blocking,
        } = self;
        Ok(match non_blocking {
            Some(nb) => {
                let (mw, gd) = nb.builder().finish(writer.build_writer()?);
                (
                    tracing_subscriber::fmt::writer::BoxMakeWriter::new(mw),
                    Some(gd),
                )
            }
            None => (
                tracing_subscriber::fmt::writer::BoxMakeWriter::new(writer.build_make_writer()?),
                None,
            ),
        })
    }
    pub fn build_writer(self) -> Result<(Box<dyn io::Write + Send + Sync>, DynGuard), BoxError> {
        let Self {
            writer,
            non_blocking,
        } = self;
        Ok(match non_blocking {
            Some(nb) => {
                let (wt, gd) = nb.builder().finish(writer.build_writer()?);
                (Box::new(wt), DynGuard::new(gd))
            }
            None => (Box::new(writer.build_writer()?), DynGuard::new(())),
        })
    }
}

impl Writer {
    pub fn build_make_writer(
        self,
    ) -> Result<tracing_subscriber::fmt::writer::BoxMakeWriter, BoxError> {
        use tracing_subscriber::fmt::writer::BoxMakeWriter;
        match self {
            Writer::File { path, options } => Ok(BoxMakeWriter::new(options.build().open(path)?)),
            Writer::Stdout => Ok(BoxMakeWriter::new(io::stdout)),
            Writer::Stderr => Ok(BoxMakeWriter::new(io::stderr)),
            Writer::TestStdout => Ok(BoxMakeWriter::new(
                tracing_subscriber::fmt::TestWriter::new(),
            )),
            Writer::TestStderr => Ok(BoxMakeWriter::new(
                tracing_subscriber::fmt::TestWriter::with_stderr(),
            )),
            Writer::Rolling { directory, config } => Ok(BoxMakeWriter::new(
                config.unwrap_or_default().builder().build(directory)?,
            )),
        }
    }
    pub fn build_writer(self) -> Result<Box<dyn io::Write + Send + Sync>, BoxError> {
        match self {
            Writer::File { path, options } => Ok(Box::new(options.build().open(path)?)),
            Writer::Stdout => Ok(Box::new(io::stdout())),
            Writer::Stderr => Ok(Box::new(io::stderr())),
            Writer::TestStdout => Ok(Box::new(tracing_subscriber::fmt::TestWriter::new())),
            Writer::TestStderr => Ok(Box::new(tracing_subscriber::fmt::TestWriter::with_stderr())),
            Writer::Rolling { directory, config } => Ok(Box::new(
                config.unwrap_or_default().builder().build(directory)?,
            )),
        }
    }
}

impl Rolling {
    pub fn builder(self) -> tracing_appender::rolling::Builder {
        let Self {
            rotation,
            filename_prefix,
            filename_suffix,
            max_log_files,
        } = self;
        tracing_appender::rolling::Builder::new()
            .tap_opt(rotation, |b, v| b.rotation(v))
            .tap_opt(filename_prefix, |b, v| b.filename_prefix(v))
            .tap_opt(filename_suffix, |b, v| b.filename_suffix(v))
            .tap_opt(max_log_files, |b, v| b.max_log_files(v))
    }
}

convert_enum! {
    _Rotation = tracing_appender::rolling::Rotation {
        [_Rotation::Minutely] = [tracing_appender::rolling::Rotation::MINUTELY]
        [_Rotation::Hourly]   = [tracing_appender::rolling::Rotation::HOURLY]
        [_Rotation::Daily]    = [tracing_appender::rolling::Rotation::DAILY]
        [_Rotation::Weekly]   = [tracing_appender::rolling::Rotation::WEEKLY]
        [_Rotation::Never]    = [tracing_appender::rolling::Rotation::NEVER]
    }
}

impl NonBlocking {
    pub fn builder(self) -> tracing_appender::non_blocking::NonBlockingBuilder {
        let Self {
            buffered_lines_limit,
            lossy,
            thread_name,
        } = self;
        tracing_appender::non_blocking::NonBlockingBuilder::default()
            .tap_opt(buffered_lines_limit, |b, v| b.buffered_lines_limit(v))
            .tap_opt(lossy, |b, v| b.lossy(v))
            .tap_opt(thread_name, |b, v| b.thread_name(&v))
    }
}

serde! {
    #[derive(Default)]
    pub struct Targets {
        #[serde_as(as = "BTreeMap<_, FromInto<_LevelFilter>>")]
        pub targets: BTreeMap<String, tracing::metadata::LevelFilter>,
        #[serde_as(as = "Option<FromInto<_LevelFilter>>")]
        pub default: Option<tracing::metadata::LevelFilter>,
    }
    #[derive(Default)]
    pub struct EnvFilterDirectives {
        #[serde_as(as = "Vec<DisplayFromStr>")]
        pub directives: Vec<tracing_subscriber::filter::Directive>,
        pub regex: Option<bool>,
    }
    pub struct EnvFilterFromEnv {
        pub from_env: TrueOr<String>,
        #[serde_as(as = "Option<DisplayFromStr>")]
        pub default: Option<tracing_subscriber::filter::Directive>,
        pub regex: Option<bool>,
    }
}

impl Default for EnvFilterFromEnv {
    fn default() -> Self {
        Self {
            from_env: TrueOr::True,
            default: Default::default(),
            regex: Default::default(),
        }
    }
}

impl Targets {
    pub fn build(self) -> tracing_subscriber::filter::Targets {
        let Self { targets, default } = self;
        tracing_subscriber::filter::Targets::new()
            .tap_opt(default, |b, v| b.with_default(v))
            .with_targets(targets)
    }
}

impl EnvFilterDirectives {
    pub fn build(self) -> tracing_subscriber::EnvFilter {
        let Self { directives, regex } = self;
        directives.into_iter().fold(
            tracing_subscriber::EnvFilter::builder()
                .tap_opt(regex, |b, v| b.with_regex(v))
                .parse_lossy(""),
            |b, v| b.add_directive(v),
        )
    }
}

impl EnvFilterFromEnv {
    pub fn build(self) -> tracing_subscriber::EnvFilter {
        let Self {
            from_env,
            default,
            regex,
        } = self;
        tracing_subscriber::EnvFilter::builder()
            .tap_opt(from_env.into_option(), |b, v| b.with_env_var(v))
            .tap_opt(default, |b, v| b.with_default_directive(v))
            .tap_opt(regex, |b, v| b.with_regex(v))
            .from_env_lossy()
    }
}

serde! {
    enum _Level {
        Error,
        Warn,
        Info,
        Debug,
        Trace,
    }
    enum _LevelFilter {
        Off,
        Error,
        Warn,
        Info,
        Debug,
        Trace,
    }
}

convert_enum! {
    _Level = tracing::Level {
        [_Level::Error] = [tracing::Level::ERROR]
        [_Level::Warn]  = [tracing::Level::WARN]
        [_Level::Info]  = [tracing::Level::INFO]
        [_Level::Debug] = [tracing::Level::DEBUG]
        [_Level::Trace] = [tracing::Level::TRACE]
    }
    _LevelFilter = tracing::metadata::LevelFilter {
        [_LevelFilter::Off]   = [tracing::metadata::LevelFilter::OFF]
        [_LevelFilter::Error] = [tracing::metadata::LevelFilter::ERROR]
        [_LevelFilter::Warn]  = [tracing::metadata::LevelFilter::WARN]
        [_LevelFilter::Info]  = [tracing::metadata::LevelFilter::INFO]
        [_LevelFilter::Debug] = [tracing::metadata::LevelFilter::DEBUG]
        [_LevelFilter::Trace] = [tracing::metadata::LevelFilter::TRACE]
    }
}

serde! {
    pub struct JournaldLayer {
        pub options: Option<JournaldOptions>,
        pub filter: Option<AnyFilter>,
    }

    #[derive(Default)]
    pub struct JournaldOptions {
        pub field_prefix: Option<FalseOr<String>>,
        pub syslog_identifer: Option<String>,
        #[serde(default, skip_serializing_if = "is_empty")]
        pub custom_fields: BTreeMap<String, OrHex<String, Vec<u8>>>,
        #[serde(default, skip_serializing_if = "is_empty")]
        #[serde_as(as = "BTreeMap<FromInto<_Level>, FromInto<_Priority>>")]
        pub priority_mapping: BTreeMap<tracing::Level, tracing_journald::Priority>,
    }

    enum _Priority {
        Emergency,
        Alert,
        Critical,
        Error,
        Warning,
        Notice,
        Informational,
        Debug,
    }
}

impl JournaldLayer {
    #[expect(clippy::type_complexity)]
    pub fn build<S>(
        self,
    ) -> Result<
        tracing_subscriber::filter::Filtered<
            tracing_journald::Layer,
            Box<dyn tracing_subscriber::layer::Filter<S> + Send + Sync>,
            S,
        >,
        BoxError,
    >
    where
        S: tracing::Subscriber + for<'a> tracing_subscriber::registry::LookupSpan<'a>,
    {
        let Self { options, filter } = self;
        Ok(options
            .unwrap_or_default()
            .apply(tracing_journald::layer()?)
            .with_filter(Box::new(filter.map(|it| it.build()))))
    }
}

impl JournaldOptions {
    pub fn apply(self, to: tracing_journald::Layer) -> tracing_journald::Layer {
        let Self {
            field_prefix,
            syslog_identifer,
            custom_fields,
            priority_mapping,
        } = self;
        to.tap_opt(field_prefix, |b, v| b.with_field_prefix(v.into_option()))
            .tap_opt(syslog_identifer, |b, v| b.with_syslog_identifier(v))
            .with_custom_fields(custom_fields.into_iter().map(|(k, v)| {
                (
                    k,
                    match v {
                        OrHex::Inline(s) => s.into_bytes(),
                        OrHex::Hex { hex } => hex,
                    },
                )
            }))
            .with_priority_mappings({
                let mut m = tracing_journald::PriorityMappings::default();
                for (k, v) in priority_mapping {
                    match k {
                        tracing::Level::ERROR => m.error = v,
                        tracing::Level::WARN => m.warn = v,
                        tracing::Level::INFO => m.info = v,
                        tracing::Level::DEBUG => m.debug = v,
                        tracing::Level::TRACE => m.trace = v,
                    }
                }
                m
            })
    }
}

convert_enum! {
    _Priority = tracing_journald::Priority {
        [_Priority::Emergency]     = [tracing_journald::Priority::Emergency]
        [_Priority::Alert]         = [tracing_journald::Priority::Alert]
        [_Priority::Critical]      = [tracing_journald::Priority::Critical]
        [_Priority::Error]         = [tracing_journald::Priority::Error]
        [_Priority::Warning]       = [tracing_journald::Priority::Warning]
        [_Priority::Notice]        = [tracing_journald::Priority::Notice]
        [_Priority::Informational] = [tracing_journald::Priority::Informational]
        [_Priority::Debug]         = [tracing_journald::Priority::Debug]
    }
}

serde! {
    pub struct FlameLayer {
        pub writer: Writer,
        pub options: Option<FlameOptions>,
        pub filter: Option<AnyFilter>,
    }

    #[derive(Default)]
    pub struct FlameOptions {
        pub empty_samples: Option<bool>,
        pub threads_collapsed: Option<bool>,
        pub module_path: Option<bool>,
        pub file_and_line: Option<bool>,
    }
}

impl FlameLayer {
    #[expect(clippy::type_complexity)]
    pub fn build<S>(
        self,
    ) -> Result<
        tracing_subscriber::filter::Filtered<
            tracing_flame::FlameLayer<S, Box<dyn io::Write + Send + Sync>>,
            Box<dyn tracing_subscriber::layer::Filter<S> + Send + Sync>,
            S,
        >,
        BoxError,
    >
    where
        S: tracing::Subscriber + for<'a> tracing_subscriber::registry::LookupSpan<'a>,
    {
        let Self {
            writer,
            options,
            filter,
        } = self;
        Ok(options
            .unwrap_or_default()
            .apply(tracing_flame::FlameLayer::new(writer.build_writer()?))
            .with_filter(Box::new(filter.map(|it| it.build()))))
    }
}

impl FlameOptions {
    pub fn apply<S, W>(self, to: tracing_flame::FlameLayer<S, W>) -> tracing_flame::FlameLayer<S, W>
    where
        S: tracing::Subscriber + for<'a> tracing_subscriber::registry::LookupSpan<'a>,
        W: io::Write + 'static,
    {
        let Self {
            empty_samples,
            threads_collapsed,
            module_path,
            file_and_line,
        } = self;
        to.tap_opt(empty_samples, |b, v| b.with_empty_samples(v))
            .tap_opt(threads_collapsed, |b, v| b.with_threads_collapsed(v))
            .tap_opt(module_path, |b, v| b.with_module_path(v))
            .tap_opt(file_and_line, |b, v| b.with_file_and_line(v))
    }
}

serde! {
    #[derive(Default)]
    pub struct ChromeLayer {
        pub writer: Option<Writer>,
        pub options: Option<ChromeOptions>,
        pub filter: Option<AnyFilter>,
    }
    #[derive(Default)]
    pub struct ChromeOptions {
        pub include_args: Option<bool>,
        pub include_locations: Option<bool>,
        pub trace_style: Option<TraceStyle>,
    }
    pub enum TraceStyle {
        Threaded,
        Async,
    }
}

impl ChromeLayer {
    #[expect(clippy::type_complexity)]
    pub fn builder<S>(
        self,
    ) -> Result<
        (
            tracing_subscriber::filter::Filtered<
                tracing_chrome::ChromeLayer<S>,
                Box<dyn tracing_subscriber::layer::Filter<S> + Send + Sync>,
                S,
            >,
            tracing_chrome::FlushGuard,
        ),
        BoxError,
    >
    where
        S: tracing::Subscriber
            + for<'span> tracing_subscriber::registry::LookupSpan<'span>
            + Send
            + Sync,
    {
        let Self {
            writer,
            options,
            filter,
        } = self;

        let wt = writer
            .unwrap_or_else(|| {
                let path = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
                    Ok(d) => format!("./trace-{}.json", d.as_micros()),
                    Err(e) => format!("./trace--{}.json", e.duration().as_micros()),
                };

                Writer::File {
                    path: PathBuf::from(path),
                    options: OpenOptions {
                        create: Some(true),
                        truncate: Some(true),
                        write: Some(true),
                        ..Default::default()
                    },
                }
            })
            .build_writer()?;
        let (lyr, gd) = options
            .unwrap_or_default()
            .apply(tracing_chrome::ChromeLayerBuilder::new().writer(wt))
            .build();
        Ok((lyr.with_filter(Box::new(filter.map(|it| it.build()))), gd))
    }
}

impl ChromeOptions {
    pub fn apply<S>(
        self,
        to: tracing_chrome::ChromeLayerBuilder<S>,
    ) -> tracing_chrome::ChromeLayerBuilder<S>
    where
        S: tracing::Subscriber + for<'span> tracing_subscriber::registry::LookupSpan<'span>,
        S: Send + Sync,
    {
        let Self {
            include_args,
            include_locations,
            trace_style,
        } = self;
        to.tap_opt(include_args, |b, v| b.include_args(v))
            .tap_opt(include_locations, |b, v| b.include_locations(v))
            .tap_opt(trace_style, |b, v| {
                b.trace_style(match v {
                    TraceStyle::Threaded => tracing_chrome::TraceStyle::Threaded,
                    TraceStyle::Async => tracing_chrome::TraceStyle::Async,
                })
            })
    }
}

serde! {
    pub struct HierarchicalLayer {
        pub writer: Option<Writer>,
        pub options: Option<HierarchicalOptions>,
        pub filter: Option<AnyFilter>,
    }

    #[derive(Default)]
    pub struct HierarchicalOptions {
        pub indent_amount: Option<usize>,
        pub ansi: Option<bool>,
        pub indent_lines: Option<bool>,
        pub targets: Option<bool>,
        pub thread_ids: Option<bool>,
        pub thread_names: Option<bool>,
        pub wraparound: Option<usize>,
        pub verbose_entry: Option<bool>,
        pub verbose_exit: Option<bool>,
        pub span_retrace: Option<bool>,
        pub deferred_spans: Option<bool>,
        pub span_modes: Option<bool>,
        pub bracketed_fields: Option<bool>,
    }
}

impl HierarchicalLayer {
    #[expect(clippy::type_complexity)]
    pub fn build<S>(
        self,
    ) -> Result<
        tracing_subscriber::filter::Filtered<
            tracing_tree::HierarchicalLayer<tracing_subscriber::fmt::writer::BoxMakeWriter>,
            Box<dyn tracing_subscriber::layer::Filter<S> + Send + Sync>,
            S,
        >,
        BoxError,
    >
    where
        S: tracing::Subscriber + for<'span> tracing_subscriber::registry::LookupSpan<'span>,
    {
        let Self {
            writer,
            options,
            filter,
        } = self;
        Ok(options
            .unwrap_or_default()
            .apply(
                tracing_tree::HierarchicalLayer::default()
                    .with_writer(writer.unwrap_or(Writer::Stdout).build_make_writer()?),
            )
            .with_filter(Box::new(filter.map(|it| it.build()))))
    }
}

impl HierarchicalOptions {
    pub fn apply<W, T>(
        self,
        to: tracing_tree::HierarchicalLayer<W, T>,
    ) -> tracing_tree::HierarchicalLayer<W, T>
    where
        W: for<'a> tracing_subscriber::fmt::MakeWriter<'a> + 'static,
        T: tracing_tree::time::FormatTime,
    {
        let Self {
            indent_amount,
            ansi,
            indent_lines,
            targets,
            thread_ids,
            thread_names,
            wraparound,
            verbose_entry,
            verbose_exit,
            span_retrace,
            deferred_spans,
            span_modes,
            bracketed_fields,
        } = self;
        to.tap_opt(indent_amount, |b, v| b.with_indent_amount(v))
            .tap_opt(ansi, |b, v| b.with_ansi(v))
            .tap_opt(indent_lines, |b, v| b.with_indent_lines(v))
            .tap_opt(targets, |b, v| b.with_targets(v))
            .tap_opt(thread_ids, |b, v| b.with_thread_ids(v))
            .tap_opt(thread_names, |b, v| b.with_thread_names(v))
            .tap_opt(wraparound, |b, v| b.with_wraparound(v))
            .tap_opt(verbose_entry, |b, v| b.with_verbose_entry(v))
            .tap_opt(verbose_exit, |b, v| b.with_verbose_exit(v))
            .tap_opt(span_retrace, |b, v| b.with_span_retrace(v))
            .tap_opt(deferred_spans, |b, v| b.with_deferred_spans(v))
            .tap_opt(span_modes, |b, v| b.with_span_modes(v))
            .tap_opt(bracketed_fields, |b, v| b.with_bracketed_fields(v))
    }
}
