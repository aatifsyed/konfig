use crate::*;

use std::{
    collections::{BTreeMap, HashMap},
    str::FromStr,
    time::Duration,
};

use opentelemetry::{KeyValue, metrics::MeterProvider as _, trace::TracerProvider as _};

use opentelemetry_otlp::{
    Compression, ExporterBuildError, HasExportConfig, HasHttpConfig, HasTonicConfig,
    MetricExporterBuilder, Protocol, SpanExporterBuilder, WithExportConfig, WithHttpConfig,
    WithTonicConfig as _,
};
use opentelemetry_sdk::{
    metrics::{MeterProviderBuilder, PeriodicReader, PeriodicReaderBuilder},
    resource::ResourceBuilder,
    trace::{SdkTracerProvider, TracerProviderBuilder},
};

serde! {
    pub struct InstrumentationScope {
        #[serde(default, skip_serializing_if = "is_empty")]
        pub name: String,
        pub version: Option<String>,
        pub schema_url: Option<String>,
        #[serde(default, skip_serializing_if = "is_empty")]
        pub attributes: BTreeMap<String, Value>,
    }

    pub struct Resource {
        #[serde(default, skip_serializing_if = "is_empty")]
        pub attributes: BTreeMap<String, Value>,
        pub schema_url: Option<String>,
        pub detect: Option<bool>,
    }

    pub struct ExportConfig {
        pub endpoint: Option<String>,
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub timeout: Option<Duration>,
        // #[serde(with = "Option<_Protocol>")]
        // #[schemars(with = "_Protocol")]
        // pub protocol: Option<opentelemetry_otlp::Protocol>,
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

    #[serde(remote = "opentelemetry_otlp::Protocol")]
    enum _Protocol {
        Grpc,
        HttpBinary,
        HttpJson,
    }

    #[serde(remote = "opentelemetry_otlp::Compression")]
    enum _Compression {
        Gzip,
        Zstd,
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
            .with_attributes(attributes.into_iter().map(|(k, v)| KeyValue::new(k, v)))
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
        .with_attributes(attributes.into_iter().map(|(k, v)| KeyValue::new(k, v)))
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
