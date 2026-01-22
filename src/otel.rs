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
        pub scope: InstrumentationScope,
        pub provider: LoggerProvider,
    }

    pub struct LoggerProvider {
        #[serde(default)]
        pub resource: Resource,
        #[serde(default, skip_serializing_if = "is_empty")]
        pub processors: Vec<LogProcessor>,
    }

    pub struct LogProcessor {
        pub batch: Option<LoggerBatchConfig>,
        pub exporter: LogExporter,
    }

    pub enum LogExporter {
        Http {
            #[serde(default)]
            export: ExportConfig,
            #[serde(default)]
            config: HttpConfig,
        },
        Tonic {
            #[serde(default)]
            export: ExportConfig,
            #[serde(default)]
            config: TonicConfig,
        }
    }

    pub struct LoggerBatchConfig {
        pub max_queue_size: Option<usize>,
        #[serde_as(as = "Option<AsHumanDuration>")]
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
            |b, LogProcessor { batch, exporter }| match batch {
                None => exporter.build().map(|e| b.with_simple_exporter(e)),
                Some(c) => Ok(b.with_log_processor(
                    opentelemetry_sdk::logs::BatchLogProcessor::builder(exporter.build()?)
                        .with_batch_config(c.builder().build())
                        .build(),
                )),
            },
        )
    }
}

impl LogExporter {
    pub fn build(self) -> Result<opentelemetry_otlp::LogExporter, BoxError> {
        match self {
            LogExporter::Http { export, config } => Ok(config
                .apply(export.apply(opentelemetry_otlp::LogExporter::builder().with_http()))
                .build()?),
            LogExporter::Tonic { export, config } => Ok(config
                .apply(export.apply(opentelemetry_otlp::LogExporter::builder().with_tonic()))
                .build()?),
        }
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
        #[serde_as(as = "Option<AsHumanDuration>")]
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
