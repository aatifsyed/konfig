use std::collections::HashSet;

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
        #[serde_as(as = "Option<HumanDuration>")]
        pub timeout: Option<Duration>,
        #[serde_as(as = "Option<HumanDuration>")]
        pub read_timeout: Option<Duration>,
        #[serde_as(as = "Option<HumanDuration>")]
        pub connect_timeout: Option<Duration>,
        pub connection_verbose: Option<bool>,
    }

    #[serde(into = "_ReqwestClientProxies", from = "_ReqwestClientProxies")]
    #[schemars(with = "_ReqwestClientProxies")]
    pub enum ReqwestClientProxies {
        False,
        Proxies(Vec<ReqwestClientProxy>)
    }

    #[serde(untagged)]
    enum _ReqwestClientProxies {
        False(False),
        Proxies(Vec<ReqwestClientProxy>)
    }
}

impl From<_ReqwestClientProxies> for ReqwestClientProxies {
    fn from(value: _ReqwestClientProxies) -> Self {
        match value {
            _ReqwestClientProxies::False(False) => Self::False,
            _ReqwestClientProxies::Proxies(items) => Self::Proxies(items),
        }
    }
}

impl From<ReqwestClientProxies> for _ReqwestClientProxies {
    fn from(value: ReqwestClientProxies) -> Self {
        match value {
            ReqwestClientProxies::False => Self::False(False),
            ReqwestClientProxies::Proxies(items) => Self::Proxies(items),
        }
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

    #[serde(from = "_ReqwestClientNoProxy", into = "_ReqwestClientNoProxy")]
    #[schemars(with = "_ReqwestClientNoProxy")]
    pub enum ReqwestClientNoProxy {
        FromEnv,
        String(String)
    }

    #[serde(untagged)]
    enum _ReqwestClientNoProxy {
        #[serde(rename_all = "kebab-case")]
        FromEnv {
            from_env: True,
        },
        String(String)
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

impl From<_ReqwestClientNoProxy> for ReqwestClientNoProxy {
    fn from(value: _ReqwestClientNoProxy) -> Self {
        match value {
            _ReqwestClientNoProxy::FromEnv { from_env: True } => Self::FromEnv,
            _ReqwestClientNoProxy::String(s) => Self::String(s),
        }
    }
}

impl From<ReqwestClientNoProxy> for _ReqwestClientNoProxy {
    fn from(value: ReqwestClientNoProxy) -> Self {
        match value {
            ReqwestClientNoProxy::FromEnv => Self::FromEnv { from_env: True },
            ReqwestClientNoProxy::String(s) => Self::String(s),
        }
    }
}
