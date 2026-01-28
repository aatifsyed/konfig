use crate::*;

use std::{
    collections::{BTreeMap, HashMap},
    time::Duration,
};

use opentelemetry_otlp::{
    HasExportConfig, HasHttpConfig, HasTonicConfig, WithExportConfig as _, WithHttpConfig as _,
    WithTonicConfig,
};
use serde_with::FromInto;

serde! {
    pub struct Logger {
        #[serde(default)]
        pub scope: InstrumentationScope,
        #[serde(default)]
        pub provider: LoggerProvider,
    }

    #[derive(Default)]
    pub struct LoggerProvider {
        #[serde(default)]
        pub resource: Resource,
        #[serde(default, skip_serializing_if = "is_empty")]
        pub processors: Vec<LogProcessor>,
    }

    pub struct LogProcessor {
        pub batch: Option<LoggerBatchConfig>,
        pub exporter: Exporter,
    }

    pub struct LoggerBatchConfig {
        pub max_queue_size: Option<usize>,
        #[serde_as(as = "Option<HumanTime>")]
        pub scheduled_delay: Option<Duration>,
        pub max_export_batch_size: Option<usize>,
    }
}

impl LoggerProvider {
    pub fn builder(self) -> Result<opentelemetry_sdk::logs::LoggerProviderBuilder, BoxError> {
        let Self {
            resource,
            processors,
        } = self;
        processors.into_iter().try_fold(
            opentelemetry_sdk::logs::LoggerProviderBuilder::default()
                .with_resource(resource.builder().build()),
            |b,
             LogProcessor {
                 batch,
                 exporter: Exporter { export, transport },
             }| {
                let exp = match transport {
                    Transport::Http(http) => export
                        .apply(http.apply(opentelemetry_otlp::LogExporter::builder().with_http()))
                        .build(),
                    Transport::Tonic(tonic) => export
                        .apply(tonic.apply(opentelemetry_otlp::LogExporter::builder().with_tonic()))
                        .build(),
                }?;

                match batch {
                    Some(batch) => Ok(b.with_log_processor(
                        opentelemetry_sdk::logs::BatchLogProcessor::builder(exp)
                            .with_batch_config(batch.builder().build())
                            .build(),
                    )),
                    None => Ok(b.with_simple_exporter(exp)),
                }
            },
        )
    }
}

impl LoggerBatchConfig {
    pub fn builder(self) -> opentelemetry_sdk::logs::BatchConfigBuilder {
        let Self {
            max_queue_size,
            scheduled_delay,
            max_export_batch_size,
        } = self;
        opentelemetry_sdk::logs::BatchConfigBuilder::default()
            .tap_opt(max_queue_size, |b, v| b.with_max_queue_size(v))
            .tap_opt(scheduled_delay, |b, v| b.with_scheduled_delay(v))
            .tap_opt(max_export_batch_size, |b, v| {
                b.with_max_export_batch_size(v)
            })
    }
}

serde! {
    pub struct Meter {
        #[serde(default)]
        pub scope: InstrumentationScope,
        #[serde(default)]
        pub provider: MeterProvider,
    }

    #[derive(Default)]
    pub struct MeterProvider {
        #[serde(default)]
        pub resource: Resource,
        #[serde(default, skip_serializing_if = "is_empty")]
        pub exporters: Vec<MetricExporter>,
    }

    pub struct MetricExporter {
        pub temporality: Option<MetricTemporality>,
        pub exporter: Exporter,
    }

    pub enum MetricTemporality {
        Cumulative,
        Delta,
        LowMemory
    }
}

impl MeterProvider {
    pub fn builder(self) -> Result<opentelemetry_sdk::metrics::MeterProviderBuilder, BoxError> {
        let Self {
            resource,
            exporters,
        } = self;
        exporters.into_iter().try_fold(
            opentelemetry_sdk::metrics::MeterProviderBuilder::default()
                .with_resource(resource.builder().build()),
            |mpb,
             MetricExporter {
                 temporality,
                 exporter: Exporter { export, transport },
             }| {
                let b =
                    opentelemetry_otlp::MetricExporter::builder().tap_opt(temporality, |b, v| {
                        b.with_temporality(match v {
                            MetricTemporality::Cumulative => {
                                opentelemetry_sdk::metrics::Temporality::Cumulative
                            }
                            MetricTemporality::Delta => {
                                opentelemetry_sdk::metrics::Temporality::Delta
                            }
                            MetricTemporality::LowMemory => {
                                opentelemetry_sdk::metrics::Temporality::LowMemory
                            }
                        })
                    });
                let exp = match transport {
                    Transport::Http(http) => export.apply(http.apply(b.with_http())).build(),
                    Transport::Tonic(tonic) => export.apply(tonic.apply(b.with_tonic())).build(),
                }?;
                Ok(mpb.with_periodic_exporter(exp))
            },
        )
    }
}

serde! {
    pub struct Tracer {
        #[serde(default)]
        pub scope: InstrumentationScope,
        #[serde(default)]
        pub provider: TracerProvider,
    }

    #[derive(Default)]
    pub struct TracerProvider {
        #[serde(default)]
        pub resource: Resource,
        #[serde(default)]
        pub sampler: Sampler,
        #[serde(default, skip_serializing_if = "is_empty")]
        pub processors: Vec<SpanProcessor>,

        pub max_events_per_span: Option<u32>,
        pub max_attributes_per_span: Option<u32>,
        pub max_links_per_span: Option<u32>,
        pub max_attributes_per_event: Option<u32>,
        pub max_attributes_per_link: Option<u32>,
    }

    pub struct SpanProcessor {
        pub batch: Option<SpanBatchConfig>,
        pub exporter: Exporter,
    }

    pub struct SpanBatchConfig {
        pub max_queue_size: Option<usize>,
        #[serde_as(as = "Option<HumanTime>")]
        pub scheduled_delay: Option<Duration>,
        pub max_export_batch_size: Option<usize>,
    }

    #[derive(Default)]
    pub enum Sampler {
        #[default]
        Always,
        Never,
        Ratio(
            #[serde_as(as = "BoundedFloat<0, 1>")]
            f64
        ),
    }
}

impl TracerProvider {
    pub fn builder(self) -> Result<opentelemetry_sdk::trace::TracerProviderBuilder, BoxError> {
        let Self {
            resource,
            sampler,
            processors,

            max_events_per_span,
            max_attributes_per_span,
            max_links_per_span,
            max_attributes_per_event,
            max_attributes_per_link,
        } = self;
        let b = opentelemetry_sdk::trace::TracerProviderBuilder::default()
            .with_resource(resource.builder().build())
            .tap_opt(max_events_per_span, |b, v| b.with_max_events_per_span(v))
            .tap_opt(max_attributes_per_span, |b, v| {
                b.with_max_attributes_per_span(v)
            })
            .tap_opt(max_links_per_span, |b, v| b.with_max_links_per_span(v))
            .tap_opt(max_attributes_per_event, |b, v| {
                b.with_max_attributes_per_event(v)
            })
            .tap_opt(max_attributes_per_link, |b, v| {
                b.with_max_attributes_per_link(v)
            })
            .with_sampler(sampler.build());

        processors.into_iter().try_fold(
            b,
            |b,
             SpanProcessor {
                 batch,
                 exporter: Exporter { export, transport },
             }| {
                let exp = match transport {
                    Transport::Http(http) => export
                        .apply(http.apply(opentelemetry_otlp::SpanExporter::builder().with_http()))
                        .build()?,
                    Transport::Tonic(tonic) => export
                        .apply(
                            tonic.apply(opentelemetry_otlp::SpanExporter::builder().with_tonic()),
                        )
                        .build()?,
                };

                match batch {
                    Some(batch) => Ok(b.with_span_processor(
                        opentelemetry_sdk::trace::BatchSpanProcessor::builder(exp)
                            .with_batch_config(batch.builder().build())
                            .build(),
                    )),
                    None => Ok(b.with_simple_exporter(exp)),
                }
            },
        )
    }
}

impl SpanBatchConfig {
    pub fn builder(self) -> opentelemetry_sdk::trace::BatchConfigBuilder {
        let Self {
            max_queue_size,
            scheduled_delay,
            max_export_batch_size,
        } = self;
        opentelemetry_sdk::trace::BatchConfigBuilder::default()
            .tap_opt(max_queue_size, |b, v| b.with_max_queue_size(v))
            .tap_opt(scheduled_delay, |b, v| b.with_scheduled_delay(v))
            .tap_opt(max_export_batch_size, |b, v| {
                b.with_max_export_batch_size(v)
            })
    }
}

impl Sampler {
    pub fn build(self) -> opentelemetry_sdk::trace::Sampler {
        match self {
            Sampler::Always => opentelemetry_sdk::trace::Sampler::AlwaysOn,
            Sampler::Never => opentelemetry_sdk::trace::Sampler::AlwaysOff,
            Sampler::Ratio(it) => opentelemetry_sdk::trace::Sampler::TraceIdRatioBased(it),
        }
    }
}

serde! {
    #[derive(Default)]
    pub struct InstrumentationScope {
        #[serde(default, skip_serializing_if = "is_empty")]
        pub name: String,
        pub version: Option<String>,
        pub schema_url: Option<String>,
        #[serde(default, skip_serializing_if = "is_empty")]
        pub attributes: BTreeMap<String, Value>,
    }

    #[derive(Default)]
    pub struct Resource {
        #[serde(default, skip_serializing_if = "is_empty")]
        pub attributes: BTreeMap<String, Value>,
        pub schema_url: Option<String>,
        pub detect: Option<bool>,
    }

    #[derive(Default)]
    pub struct ExportConfig {
        pub endpoint: Option<String>,
        #[serde_as(as = "Option<HumanTime>")]
        pub timeout: Option<Duration>,
        #[serde_as(as = "Option<FromInto<_Protocol>>")]
        pub protocol: Option<opentelemetry_otlp::Protocol>,
    }

    #[derive(Default)]
    pub struct HttpConfig {
        #[serde_as(as = "Option<FromInto<_Compression>>")]
        pub compression: Option<opentelemetry_otlp::Compression>,
        #[serde(default, skip_serializing_if = "is_empty")]
        pub headers: HashMap<String, String>,
    }

    #[derive(Default)]
    pub struct TonicConfig {
        #[serde_as(as = "Option<FromInto<_Compression>>")]
        pub compression: Option<opentelemetry_otlp::Compression>,
        #[serde_as(as = "Option<HeaderMap>")]
        pub headers: Option<http::HeaderMap>,
    }

    pub struct Exporter {
        #[serde(default)]
        pub export: ExportConfig,
        pub transport: Transport,
    }

    pub enum Transport {
        Http(HttpConfig),
        Tonic(TonicConfig),
    }


    #[serde(untagged)]
    pub enum Value {
        Bool(bool),
        I64(i64),
        F64(f64),
        String(String),
        Array(Array),
    }

    #[serde(untagged)]
    pub enum Array {
        Bool(Vec<bool>),
        I64(Vec<i64>),
        F64(Vec<f64>),
        String(Vec<String>),
    }

    enum _Protocol {
        Grpc,
        HttpBinary,
        HttpJson,
    }

    enum _Compression {
        Gzip,
        Zstd,
    }
}

convert_enum! {
    _Protocol = opentelemetry_otlp::Protocol {
        [_Protocol::Grpc]       = [opentelemetry_otlp::Protocol::Grpc]
        [_Protocol::HttpBinary] = [opentelemetry_otlp::Protocol::HttpBinary]
        [_Protocol::HttpJson]   = [opentelemetry_otlp::Protocol::HttpJson]
    }
    _Compression = opentelemetry_otlp::Compression {
        [_Compression::Gzip] = [opentelemetry_otlp::Compression::Gzip]
        [_Compression::Zstd] = [opentelemetry_otlp::Compression::Zstd]
    }
}

impl InstrumentationScope {
    pub fn builder(self) -> opentelemetry::InstrumentationScopeBuilder {
        let Self {
            name,
            version,
            schema_url,
            attributes,
        } = self;
        opentelemetry::InstrumentationScope::builder(name)
            .tap_opt(version, |b, v| b.with_version(v))
            .tap_opt(schema_url, |b, v| b.with_schema_url(v))
            .with_attributes(
                attributes
                    .into_iter()
                    .map(|(k, v)| opentelemetry::KeyValue::new(k, v)),
            )
    }
}

impl Resource {
    pub fn builder(self) -> opentelemetry_sdk::resource::ResourceBuilder {
        let Self {
            attributes,
            schema_url,
            detect,
        } = self;
        match detect.unwrap_or(true) {
            true => opentelemetry_sdk::Resource::builder(),
            false => opentelemetry_sdk::Resource::builder_empty(),
        }
        .tap_opt(schema_url, |b, v| b.with_schema_url([], v))
        .with_attributes(
            attributes
                .into_iter()
                .map(|(k, v)| opentelemetry::KeyValue::new(k, v)),
        )
    }
}

impl From<Value> for opentelemetry::Value {
    fn from(value: Value) -> Self {
        match value {
            Value::Bool(it) => Self::Bool(it),
            Value::I64(it) => Self::I64(it),
            Value::F64(it) => Self::F64(it),
            Value::String(it) => Self::String(it.into()),
            Value::Array(it) => Self::Array(it.into()),
        }
    }
}

impl From<Array> for opentelemetry::Array {
    fn from(value: Array) -> Self {
        match value {
            Array::Bool(items) => Self::Bool(items),
            Array::I64(items) => Self::I64(items),
            Array::F64(items) => Self::F64(items),
            Array::String(items) => Self::String(items.into_iter().map(Into::into).collect()),
        }
    }
}

impl ExportConfig {
    pub fn apply<T: HasExportConfig>(self, mut to: T) -> T {
        let Self {
            endpoint,
            timeout,
            protocol,
        } = self;
        let protocol = protocol.unwrap_or(to.export_config().protocol);
        to.with_export_config(opentelemetry_otlp::ExportConfig {
            endpoint,
            protocol,
            timeout,
        })
    }
}

impl HttpConfig {
    pub fn apply<T: HasHttpConfig>(self, to: T) -> T {
        let Self {
            compression,
            headers,
        } = self;
        to.tap_opt(compression, |b, v| b.with_compression(v))
            .with_headers(headers)
    }
}

impl TonicConfig {
    pub fn apply<T: HasTonicConfig>(self, to: T) -> T {
        let Self {
            compression,
            headers,
        } = self;

        to.tap_opt(compression, |b, v| b.with_compression(v))
            .tap_opt(headers, |b, v| {
                let mut m = opentelemetry_otlp::tonic_types::metadata::MetadataMap::new();
                *m.as_mut() = v;
                b.with_metadata(m)
            })
    }
}
