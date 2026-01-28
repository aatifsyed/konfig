use std::{io, path::PathBuf, sync::Arc};

use crate::*;

use ::tracing;
use serde_with::{DisplayFromStr, FromInto};
use tracing_subscriber::Layer as _;

serde! {
    #[derive(Default)]
    pub struct FmtLayer {
        pub format: Option<FmtLayerFormat>,
        pub writer: Option<Writer>,
        pub non_blocking: Option<NonBlocking>,
        pub filter: Option<FmtLayerFilter>,
    }

    #[derive(Default)]
    pub struct FmtLayerFormat {
        pub formatter: Option<Formatter>,
        pub options: Option<FormatOptions>,
        pub span_events: Option<FmtSpan>,
    }

    #[serde(untagged)]
    pub enum FmtLayerFilter {
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
            DynGuard,
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
        let writer = writer.unwrap_or_default();
        let (mw, gd) = match non_blocking {
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
        };
        let lyr = format
            .unwrap_or_default()
            .apply(tracing_subscriber::fmt::layer().with_writer(mw))
            .with_filter(
                filter
                    .unwrap_or(FmtLayerFilter::Level {
                        level: tracing::metadata::LevelFilter::WARN,
                    })
                    .build(),
            );
        Ok((lyr, DynGuard(Box::new(gd))))
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
        } = self;
        let options = options.unwrap_or_default();
        let span_events = span_events.unwrap_or_default().build();
        match formatter.unwrap_or_default() {
            Formatter::Full => to
                .fmt_fields(tracing_subscriber::fmt::format::DefaultFields::new())
                .event_format(options.apply(tracing_subscriber::fmt::format::Format::default()))
                .with_span_events(span_events)
                .boxed(),
            Formatter::Compact => to
                .fmt_fields(tracing_subscriber::fmt::format::DefaultFields::new())
                .event_format(
                    options.apply(tracing_subscriber::fmt::format::Format::default().compact()),
                )
                .with_span_events(span_events)
                .boxed(),
            Formatter::Pretty => to
                .fmt_fields(tracing_subscriber::fmt::format::PrettyFields::new())
                .event_format(
                    options.apply(tracing_subscriber::fmt::format::Format::default().pretty()),
                )
                .with_span_events(span_events)
                .boxed(),
            Formatter::Json(json) => {
                to.fmt_fields(tracing_subscriber::fmt::format::JsonFields::new())
                    .event_format(options.apply(
                        json.apply(tracing_subscriber::fmt::format::Format::default().json()),
                    ))
                    .with_span_events(span_events)
                    .boxed()
            }
        }
    }
}

impl FmtLayerFilter {
    pub fn build<S: tracing::Subscriber>(
        self,
    ) -> Box<dyn tracing_subscriber::layer::Filter<S> + Send + Sync> {
        match self {
            FmtLayerFilter::Level { level } => Box::new(level),
            FmtLayerFilter::Targets(it) => Box::new(it.build()),
            FmtLayerFilter::EnvFilterDirectives(it) => Box::new(it.build()),
            FmtLayerFilter::EnvFilterFromEnv(it) => Box::new(it.build()),
        }
    }
}

#[must_use]
#[expect(dead_code)]
pub struct DynGuard(Box<dyn Send + Sync>);

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

    #[derive(Default)]
    pub enum Writer {
        File {
            path: PathBuf,
            options: OpenOptions,
        },
        #[default]
        Stdout,
        Stderr,
        TestStdout,
        TestStderr,
        Rolling {
            directory: PathBuf,
            config: Option<Rolling>,
        }
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
