use std::{collections::HashSet, net::IpAddr};

use url::Url;

use crate::*;

serde! {
    #[derive(Default)]
    pub struct ReqwestClient {
        #[serde_as(as = "Option<HeaderMap>")]
        pub headers: Option<http::HeaderMap>,
        pub cookie_store: Option<bool>,
        pub gzip: Option<bool>,
        pub brotli: Option<bool>,
        pub zstd: Option<bool>,
        pub deflate: Option<bool>,
        pub redirect: Option<usize>,
        pub referer: Option<bool>,
        pub retry: Option<ReqwestClientRetry>,
        pub proxies: Option<ReqwestClientProxies>,
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub timeout: Option<Duration>,
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub read_timeout: Option<Duration>,
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub connect_timeout: Option<Duration>,
        pub connection_verbose: Option<bool>,
        pub pool_idle_timeout: Option<ReqwestClientOptionalDuration>,
        pub pool_max_idle_per_host: Option<usize>,
        pub http1_title_case_headers: Option<bool>,
        pub http1_allow_obsolete_multiline_headers_in_responses: Option<bool>,
        pub http1_ignore_invalid_headers_in_responses: Option<bool>,
        pub http1_allow_spaces_after_header_name_in_responses: Option<bool>,
        pub http1_only: Option<bool>,
        pub http09_responses: Option<bool>,
        pub http2_prior_knowledge: Option<bool>,
        pub http2_initial_stream_window_size: Option<ReqwestClientOptionalSize>,
        pub http2_initial_connection_window_size: Option<ReqwestClientOptionalSize>,
        pub http2_adaptive_window: Option<bool>,
        pub http2_max_frame_size: Option<ReqwestClientOptionalSize>,
        pub http2_max_list_size: Option<u32>,
        pub http2_keep_alive_interval: Option<ReqwestClientOptionalDuration>,
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub http2_keep_alive_timeout: Option<Duration>,
        pub http2_keep_alive_while_idle: Option<bool>,
        pub tcp_nodelay: Option<bool>,
        pub local_address: Option<ReqwestClientOptionalIpAddr>,
        pub interface: Option<String>,
        pub tcp_keepalive: Option<ReqwestClientOptionalDuration>,
        pub tcp_keepalive_internal: Option<ReqwestClientOptionalDuration>,
        pub tcp_keepalive_retries: Option<ReqwestClientOptionalSize>,
        pub tcp_user_timeout: Option<ReqwestClientOptionalDuration>,
        pub unix_socket: Option<String>,
        pub tls_certs: Option<ReqwestClientTlsCerts>,
    }

    #[serde(into = "Untagged<False, Vec<ReqwestClientProxy>>", from = "Untagged<False, Vec<ReqwestClientProxy>>")]
    #[schemars(with = "Untagged<False, Vec<ReqwestClientProxy>>")]
    pub enum ReqwestClientProxies {
        False,
        Proxies(Vec<ReqwestClientProxy>),
    }

    #[serde(into = "Untagged<False, ViaHumanDuration>", from = "Untagged<False, ViaHumanDuration>")]
    #[schemars(with = "Untagged<False, ViaHumanDuration>")]
    pub enum ReqwestClientOptionalDuration {
        False,
        Duration(Duration),
    }

    #[serde(into = "Untagged<False, u32>", from = "Untagged<False, u32>")]
    #[schemars(with = "Untagged<False, u32>")]
    pub enum ReqwestClientOptionalSize {
        False,
        Size(u32),
    }

    #[serde(into = "Untagged<False, IpAddr>", from = "Untagged<False, IpAddr>")]
    #[schemars(with = "Untagged<False, IpAddr>")]
    pub enum ReqwestClientOptionalIpAddr {
        False,
        IpAddr(IpAddr)
    }

    pub enum ReqwestClientTlsCerts {
        Merge(Vec<reqwest::tls::Certificate>),
        Only(Vec<reqwest::tls::Certificate>),
    }
}

convert_enum! {
    ReqwestClientProxies = Untagged<False, Vec<ReqwestClientProxy>> {
        [ReqwestClientProxies::False] = [Untagged::Left(False)]
        [ReqwestClientProxies::Proxies(v)] = [Untagged::Right(v)]
    }
    ReqwestClientOptionalDuration = Untagged<False, ViaHumanDuration> {
        [ReqwestClientOptionalDuration::Duration(d)] = [Untagged::Right(ViaHumanDuration(d))]
        [ReqwestClientOptionalDuration::False]       = [Untagged::Left(False)]
    }
    ReqwestClientOptionalSize = Untagged<False, u32> {
        [ReqwestClientOptionalSize::False] = [Untagged::Left(False)]
        [ReqwestClientOptionalSize::Size(u)] = [Untagged::Right(u)]
    }
    ReqwestClientOptionalIpAddr = Untagged<False, IpAddr> {
        [ReqwestClientOptionalIpAddr::False] = [Untagged::Left(False)]
        [ReqwestClientOptionalIpAddr::IpAddr(addr)] = [Untagged::Right(addr)]
    }
}

serde! {
    pub struct ReqwestClientRetry {
        pub host: ReqwestClientRetryHost,
        pub unlimited: Option<bool>,
        #[serde_as(as = "Option<BoundedFloat<0, 1000>>")]
        pub max_extra_load: Option<f32>,
        pub max_retries_per_request: Option<u32>,
    }

    #[serde(untagged)]
    pub enum ReqwestClientRetryHost {
        Host(String),
        Regex { regex: Regex },
        #[serde(rename_all = "kebab-case")]
        RegexSet { regex_set: RegexSet },
        Hosts(HashSet<String>),
    }
}

impl ReqwestClientRetry {
    pub fn builder(self) -> Result<reqwest::retry::Builder, BoxError> {
        let Self {
            host,
            unlimited,
            max_extra_load,
            max_retries_per_request,
        } = self;
        let b = match host {
            ReqwestClientRetryHost::Host(s) => reqwest::retry::for_host(s),
            ReqwestClientRetryHost::Regex { regex } => {
                struct RegexIsMatch(regex::Regex);
                impl PartialEq<&str> for RegexIsMatch {
                    fn eq(&self, other: &&str) -> bool {
                        self.0.is_match(other)
                    }
                }
                reqwest::retry::for_host(RegexIsMatch(regex.builder().build()?))
            }
            ReqwestClientRetryHost::RegexSet { regex_set } => {
                struct RegexSetIsMatch(regex::RegexSet);
                impl PartialEq<&str> for RegexSetIsMatch {
                    fn eq(&self, other: &&str) -> bool {
                        self.0.is_match(other)
                    }
                }
                reqwest::retry::for_host(RegexSetIsMatch(regex_set.builder().build()?))
            }
            ReqwestClientRetryHost::Hosts(items) => {
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
    pub struct ReqwestClientProxy {
        pub url: Url,
        pub protocol: ReqwestProxyProtocol,
        #[serde_as(as = "Option<HeaderMap>")]
        pub headers: Option<http::HeaderMap>,
        pub no_proxy: Option<ReqwestClientNoProxy>,
    }

    pub enum ReqwestProxyProtocol {
        Http,
        Https,
        Either,
    }

    #[serde(from = "Untagged<String, FromEnv>", into = "Untagged<String, FromEnv>")]
    #[schemars(with = "Untagged<String, FromEnv>")]
    pub enum ReqwestClientNoProxy {
        String(String),
        FromEnv,
    }

    struct FromEnv { from_env: True }
}

convert_enum! {
    ReqwestClientNoProxy = Untagged<String, FromEnv> {
        [ReqwestClientNoProxy::String(s)] = [Untagged::Left(s)]
        [ReqwestClientNoProxy::FromEnv]   = [Untagged::Right(FromEnv { from_env: True })]
    }
}

impl ReqwestClientProxy {
    pub fn build(self) -> reqwest::Result<reqwest::Proxy> {
        let Self {
            url,
            protocol,
            headers,
            no_proxy,
        } = self;
        Ok(match protocol {
            ReqwestProxyProtocol::Http => reqwest::Proxy::http(url),
            ReqwestProxyProtocol::Https => reqwest::Proxy::https(url),
            ReqwestProxyProtocol::Either => reqwest::Proxy::all(url),
        }?
        .tap_opt(headers, |b, v| b.headers(v))
        .tap_opt(no_proxy, |b, v| match v {
            ReqwestClientNoProxy::FromEnv => b.no_proxy(reqwest::NoProxy::from_env()),
            ReqwestClientNoProxy::String(s) => b.no_proxy(reqwest::NoProxy::from_string(&s)),
        }))
    }
}

#[test]
fn reqwest_client_no_proxy() {
    assert_round_trip::<ReqwestClientNoProxy>(
        json!("hello"),
        expect![[r#"
        String(
            "hello",
        )
    "#]],
    );
    assert_round_trip::<ReqwestClientNoProxy>(
        json!({"from-env": true}),
        expect![[r#"
        FromEnv
    "#]],
    );
}
