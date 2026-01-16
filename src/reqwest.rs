use std::{
    collections::{BTreeMap, HashSet},
    net::{IpAddr, SocketAddr},
};

use serde_with::TryFromInto;
use url::Url;

use crate::*;
use ::reqwest;

serde! {
    #[derive(Default)]
    pub struct Client {
        #[serde_as(as = "Option<HeaderMap>")]
        pub default_headers: Option<http::HeaderMap>,
        pub cookie_store: Option<bool>,
        pub gzip: Option<bool>,
        pub brotli: Option<bool>,
        pub zstd: Option<bool>,
        pub deflate: Option<bool>,
        pub redirect: Option<usize>,
        pub referer: Option<bool>,
        pub retry: Option<ClientRetry>,
        pub proxies: Option<FalseOr<Vec<Proxy>>>,
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub timeout: Option<Duration>,
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub read_timeout: Option<Duration>,
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub connect_timeout: Option<Duration>,
        pub connection_verbose: Option<bool>,
        #[serde_as(as = "Option<FalseOrWith<AsHumanDuration, Duration>>")]
        pub pool_idle_timeout: Option<FalseOr<Duration>>,
        pub pool_max_idle_per_host: Option<usize>,
        pub http1_title_case_headers: Option<bool>,
        pub http1_allow_obsolete_multiline_headers_in_responses: Option<bool>,
        pub http1_ignore_invalid_headers_in_responses: Option<bool>,
        pub http1_allow_spaces_after_header_name_in_responses: Option<bool>,
        pub http1_only: Option<bool>,
        pub http09_responses: Option<bool>,
        pub http2_prior_knowledge: Option<bool>,
        pub http2_initial_stream_window_size: Option<FalseOr<u32>>,
        pub http2_initial_connection_window_size: Option<FalseOr<u32>>,
        pub http2_adaptive_window: Option<bool>,
        pub http2_max_frame_size: Option<FalseOr<u32>>,
        pub http2_max_list_size: Option<u32>,
        #[serde_as(as = "Option<FalseOrWith<AsHumanDuration, Duration>>")]
        pub http2_keep_alive_interval: Option<FalseOr<Duration>>,
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub http2_keep_alive_timeout: Option<Duration>,
        pub http2_keep_alive_while_idle: Option<bool>,
        pub tcp_nodelay: Option<bool>,
        pub local_address: Option<FalseOr<IpAddr>>,
        pub interface: Option<String>,
        #[serde_as(as = "Option<FalseOrWith<AsHumanDuration, Duration>>")]
        pub tcp_keepalive: Option<FalseOr<Duration>>,
        #[serde_as(as = "Option<FalseOrWith<AsHumanDuration, Duration>>")]
        pub tcp_keepalive_internal: Option<FalseOr<Duration>>,
        pub tcp_keepalive_retries: Option<FalseOr<u32>>,
        #[serde_as(as = "Option<FalseOrWith<AsHumanDuration, Duration>>")]
        pub tcp_user_timeout: Option<FalseOr<Duration>>,
        pub unix_socket: Option<String>,
        pub tls_certs: Option<TlsCerts>,
        pub tls_crls: Option<TlsCrls>,
        pub identity: Option<String>,
        pub tls_danger_accept_invalid_hostnames: Option<bool>,
        pub tls_danger_accept_invalid_certs: Option<bool>,
        pub tls_sni: Option<bool>,
        #[serde_as(as = "Option<TryFromInto<_Version>>")]
        pub tls_version_min: Option<reqwest::tls::Version>,
        #[serde_as(as = "Option<TryFromInto<_Version>>")]
        pub tls_version_max: Option<reqwest::tls::Version>,
        pub tls_info: Option<bool>,
        pub https_only: Option<bool>,
        pub hickory_dns: Option<bool>,
        pub resolve: Option<BTreeMap<String, Vec<SocketAddr>>>,
        pub tls_backend: Option<TlsBackend>
    }

    pub enum TlsCerts {
        Merge(Vec<String>),
        Only(Vec<String>),
    }

    pub enum TlsCrls {
        Only(Vec<String>),
    }

    pub enum TlsBackend {
        Native,
        Rustls
    }

    enum _Version {
        #[serde(rename = "1.0")]
        V1_0,
        #[serde(rename = "1.1")]
        V1_1,
        #[serde(rename = "1.2")]
        V1_2,
        #[serde(rename = "1.3")]
        V1_3,
    }
}

impl Client {
    pub fn builder(self) -> Result<reqwest::ClientBuilder, BoxError> {
        let Self {
            default_headers,
            cookie_store,
            gzip,
            brotli,
            zstd,
            deflate,
            redirect,
            referer,
            retry,
            proxies,
            timeout,
            read_timeout,
            connect_timeout,
            connection_verbose,
            pool_idle_timeout,
            pool_max_idle_per_host,
            http1_title_case_headers,
            http1_allow_obsolete_multiline_headers_in_responses,
            http1_ignore_invalid_headers_in_responses,
            http1_allow_spaces_after_header_name_in_responses,
            http1_only,
            http09_responses,
            http2_prior_knowledge,
            http2_initial_stream_window_size,
            http2_initial_connection_window_size,
            http2_adaptive_window,
            http2_max_frame_size,
            http2_max_list_size,
            http2_keep_alive_interval,
            http2_keep_alive_timeout,
            http2_keep_alive_while_idle,
            tcp_nodelay,
            local_address,
            interface,
            tcp_keepalive,
            tcp_keepalive_internal,
            tcp_keepalive_retries,
            tcp_user_timeout,
            unix_socket,
            tls_certs,
            tls_crls,
            identity,
            tls_danger_accept_invalid_hostnames,
            tls_danger_accept_invalid_certs,
            tls_sni,
            tls_version_min,
            tls_version_max,
            tls_info,
            https_only,
            hickory_dns,
            resolve,
            tls_backend,
        } = self;
        Ok(reqwest::ClientBuilder::new()
            .tap_opt(default_headers, |b, v| b.default_headers(v))
            .tap_opt(cookie_store, |b, v| b.cookie_store(v))
            .tap_opt(gzip, |b, v| b.gzip(v))
            .tap_opt(brotli, |b, v| b.brotli(v))
            .tap_opt(zstd, |b, v| b.zstd(v))
            .tap_opt(deflate, |b, v| b.deflate(v))
            .tap_opt(redirect, |b, v| match v {
                0 => b.redirect(reqwest::redirect::Policy::none()),
                v => b.redirect(reqwest::redirect::Policy::limited(v)),
            })
            .tap_opt(referer, |b, v| b.referer(v))
            .try_tap_opt(retry, |b, v| v.builder().map(|v| b.retry(v)))?
            .try_tap_opt(proxies, |b, v| match v {
                FalseOr::False => Ok(b.no_proxy()),
                FalseOr::Or(proxs) => proxs
                    .into_iter()
                    .try_fold(b, |b, prox| prox.build().map(|v| b.proxy(v))),
            })?
            .tap_opt(timeout, |b, v| b.timeout(v))
            .tap_opt(read_timeout, |b, v| b.read_timeout(v))
            .tap_opt(connect_timeout, |b, v| b.connect_timeout(v))
            .tap_opt(connection_verbose, |b, v| b.connection_verbose(v))
            .tap_opt(pool_idle_timeout, |b, v| {
                b.pool_idle_timeout(v.into_option())
            })
            .tap_opt(pool_max_idle_per_host, |b, v| b.pool_max_idle_per_host(v))
            .tap_opt(http1_title_case_headers, |b, v| {
                if v { b.http1_title_case_headers() } else { b }
            })
            .tap_opt(
                http1_allow_obsolete_multiline_headers_in_responses,
                |b, v| b.http1_allow_obsolete_multiline_headers_in_responses(v),
            )
            .tap_opt(http1_ignore_invalid_headers_in_responses, |b, v| {
                b.http1_ignore_invalid_headers_in_responses(v)
            })
            .tap_opt(http1_allow_spaces_after_header_name_in_responses, |b, v| {
                b.http1_allow_spaces_after_header_name_in_responses(v)
            })
            .tap_opt(http1_only, |b, v| if v { b.http1_only() } else { b })
            .tap_opt(
                http09_responses,
                |b, v| {
                    if v { b.http09_responses() } else { b }
                },
            )
            .tap_opt(tcp_nodelay, |b, v| b.tcp_nodelay(v))
            .tap_opt(local_address, |b, v| b.local_address(v.into_option()))
            .tap_opt(interface, |b, v| b.interface(&v))
            .tap_opt(tcp_keepalive, |b, v| b.tcp_keepalive(v.into_option()))
            .tap_opt(tcp_keepalive_internal, |b, v| {
                b.tcp_keepalive_interval(v.into_option())
            })
            .tap_opt(tcp_keepalive_retries, |b, v| {
                b.tcp_keepalive_retries(v.into_option())
            })
            .tap_opt(tcp_user_timeout, |b, v| b.tcp_user_timeout(v.into_option()))
            .tap_opt(unix_socket, |b, v| {
                #[cfg(unix)]
                return b.unix_socket(v);
                #[cfg(not(unix))]
                return b;
            })
            .try_tap_opt(tls_certs, |b, v| match v {
                TlsCerts::Merge(v) => v
                    .into_iter()
                    .map(|s| reqwest::tls::Certificate::from_pem(s.as_bytes()))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|v| b.tls_certs_merge(v)),
                TlsCerts::Only(v) => v
                    .into_iter()
                    .map(|s| reqwest::tls::Certificate::from_pem(s.as_bytes()))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|v| b.tls_certs_only(v)),
            })?
            .try_tap_opt(tls_crls, |b, v| match v {
                TlsCrls::Only(v) => v
                    .into_iter()
                    .map(|s| reqwest::tls::CertificateRevocationList::from_pem(s.as_bytes()))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|v| b.tls_crls_only(v)),
            })?
            .try_tap_opt(identity, |b, v| {
                reqwest::tls::Identity::from_pem(v.as_bytes()).map(|v| b.identity(v))
            })?
            .tap_opt(tls_danger_accept_invalid_hostnames, |b, v| {
                b.tls_danger_accept_invalid_hostnames(v)
            })
            .tap_opt(tls_danger_accept_invalid_certs, |b, v| {
                b.tls_danger_accept_invalid_certs(v)
            })
            .tap_opt(tls_sni, |b, v| b.tls_sni(v))
            .tap_opt(tls_version_min, |b, v| b.tls_version_min(v))
            .tap_opt(tls_version_max, |b, v| b.tls_version_max(v))
            .tap_opt(tls_info, |b, v| b.tls_info(v))
            .tap_opt(https_only, |b, v| b.https_only(v))
            .tap_opt(http2_prior_knowledge, |b, v| {
                if v { b.http2_prior_knowledge() } else { b }
            })
            .tap_opt(http2_initial_stream_window_size, |b, v| {
                b.http2_initial_stream_window_size(v.into_option())
            })
            .tap_opt(http2_initial_connection_window_size, |b, v| {
                b.http2_initial_connection_window_size(v.into_option())
            })
            .tap_opt(http2_adaptive_window, |b, v| b.http2_adaptive_window(v))
            .tap_opt(http2_max_frame_size, |b, v| {
                b.http2_max_frame_size(v.into_option())
            })
            .tap_opt(http2_max_list_size, |b, v| b.http2_max_header_list_size(v))
            .tap_opt(http2_keep_alive_interval, |b, v| {
                b.http2_keep_alive_interval(v.into_option())
            })
            .tap_opt(http2_keep_alive_timeout, |b, v| {
                b.http2_keep_alive_timeout(v)
            })
            .tap_opt(http2_keep_alive_while_idle, |b, v| {
                b.http2_keep_alive_while_idle(v)
            })
            .tap_opt(hickory_dns, |b, v| b.hickory_dns(v))
            .tap_opt(resolve, |b, v| {
                v.into_iter()
                    .fold(b, |b, (domain, addrs)| b.resolve_to_addrs(&domain, &addrs))
            })
            .tap_opt(tls_backend, |b, v| match v {
                TlsBackend::Native => b.tls_backend_native(),
                TlsBackend::Rustls => b.tls_backend_rustls(),
            }))
    }
}

impl From<_Version> for reqwest::tls::Version {
    fn from(value: _Version) -> Self {
        match value {
            _Version::V1_0 => Self::TLS_1_0,
            _Version::V1_1 => Self::TLS_1_1,
            _Version::V1_2 => Self::TLS_1_2,
            _Version::V1_3 => Self::TLS_1_3,
        }
    }
}

impl TryFrom<reqwest::tls::Version> for _Version {
    type Error = String;
    fn try_from(value: reqwest::tls::Version) -> Result<Self, Self::Error> {
        Ok(match value {
            reqwest::tls::Version::TLS_1_0 => _Version::V1_0,
            reqwest::tls::Version::TLS_1_1 => _Version::V1_1,
            reqwest::tls::Version::TLS_1_2 => _Version::V1_2,
            reqwest::tls::Version::TLS_1_3 => _Version::V1_3,
            other => return Err(format!("uncovered version {other:?}")),
        })
    }
}

serde! {
    pub struct ClientRetry {
        pub host: RetryHost,
        pub unlimited: Option<bool>,
        #[serde_as(as = "Option<BoundedFloat<0, 1000>>")]
        pub max_extra_load: Option<f32>,
        pub max_retries_per_request: Option<u32>,
    }

    #[serde(untagged)]
    pub enum RetryHost {
        Host(String),
        Regex { regex: regex::Regex },
        #[serde(rename_all = "kebab-case")]
        RegexSet { regex_set: regex::RegexSet },
        Hosts(HashSet<String>),
    }
}

impl ClientRetry {
    pub fn builder(self) -> Result<reqwest::retry::Builder, BoxError> {
        let Self {
            host,
            unlimited,
            max_extra_load,
            max_retries_per_request,
        } = self;
        let b = match host {
            RetryHost::Host(s) => reqwest::retry::for_host(s),
            RetryHost::Regex { regex } => {
                struct RegexIsMatch(::regex::Regex);
                impl PartialEq<&str> for RegexIsMatch {
                    fn eq(&self, other: &&str) -> bool {
                        self.0.is_match(other)
                    }
                }
                reqwest::retry::for_host(RegexIsMatch(regex.builder().build()?))
            }
            RetryHost::RegexSet { regex_set } => {
                struct RegexSetIsMatch(::regex::RegexSet);
                impl PartialEq<&str> for RegexSetIsMatch {
                    fn eq(&self, other: &&str) -> bool {
                        self.0.is_match(other)
                    }
                }
                reqwest::retry::for_host(RegexSetIsMatch(regex_set.builder().build()?))
            }
            RetryHost::Hosts(items) => {
                struct SetStringIsMatch(HashSet<String>);
                impl PartialEq<&str> for SetStringIsMatch {
                    fn eq(&self, other: &&str) -> bool {
                        self.0.contains(*other)
                    }
                }
                reqwest::retry::for_host(SetStringIsMatch(items))
            }
        };
        Ok(b.tap_opt(max_extra_load, |b, v| b.max_extra_load(v))
            .tap_opt(max_retries_per_request, |b, v| b.max_retries_per_request(v))
            .tap_opt(unlimited, |b, v| match v {
                true => b.no_budget(),
                false => b,
            }))
    }
}

serde! {
    pub struct Proxy {
        pub url: Url,
        pub protocol: ProxyProtocol,
        #[serde_as(as = "Option<HeaderMap>")]
        pub headers: Option<http::HeaderMap>,
        pub no_proxy: Option<NoProxy>,
    }

    pub enum ProxyProtocol {
        Http,
        Https,
        Either,
    }

    #[serde(from = "Untagged<String, FromEnv>", into = "Untagged<String, FromEnv>")]
    #[schemars(with = "Untagged<String, FromEnv>")]
    pub enum NoProxy {
        String(String),
        FromEnv,
    }

    struct FromEnv { from_env: True }
}

convert_enum! {
    NoProxy = Untagged<String, FromEnv> {
        [NoProxy::String(s)] = [Untagged::Left(s)]
        [NoProxy::FromEnv]   = [Untagged::Right(FromEnv { from_env: True })]
    }
}

impl Proxy {
    pub fn build(self) -> reqwest::Result<reqwest::Proxy> {
        let Self {
            url,
            protocol,
            headers,
            no_proxy,
        } = self;
        Ok(match protocol {
            ProxyProtocol::Http => reqwest::Proxy::http(url),
            ProxyProtocol::Https => reqwest::Proxy::https(url),
            ProxyProtocol::Either => reqwest::Proxy::all(url),
        }?
        .tap_opt(headers, |b, v| b.headers(v))
        .tap_opt(no_proxy, |b, v| match v {
            NoProxy::FromEnv => b.no_proxy(reqwest::NoProxy::from_env()),
            NoProxy::String(s) => b.no_proxy(reqwest::NoProxy::from_string(&s)),
        }))
    }
}

#[test]
fn reqwest_client_no_proxy() {
    assert_round_trip::<NoProxy>(
        json!("hello"),
        expect![[r#"
        String(
            "hello",
        )
    "#]],
    );
    assert_round_trip::<NoProxy>(
        json!({"from-env": true}),
        expect![[r#"
        FromEnv
    "#]],
    );
}
