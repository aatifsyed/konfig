use crate::*;

serde! {
    #[derive(Default)]
    pub struct ConstantBackoff {
        #[serde_as(as = "Option<HumanDuration>")]
        pub delay: Option<Duration>,
        pub max_times: Option<BackoffMaxTimes>,
        pub jitter: Option<BackoffJitter>,
    }
}

#[test]
fn constant_backoff() {
    assert_round_trip::<ConstantBackoff>(
        json!({
            "delay": "1h",
            "max-times": false,
            "jitter": true,
        }),
        expect![[r#"
            ConstantBackoff {
                delay: Some(
                    3600s,
                ),
                max_times: Some(
                    False,
                ),
                jitter: Some(
                    True,
                ),
            }
        "#]],
    );
    assert_round_trip::<ConstantBackoff>(
        json!({
            "delay": "1h",
            "max-times": 1000,
            "jitter": {
                "seeded": 100,
            }
        }),
        expect![[r#"
            ConstantBackoff {
                delay: Some(
                    3600s,
                ),
                max_times: Some(
                    MaxTimes(
                        1000,
                    ),
                ),
                jitter: Some(
                    Seeded(
                        100,
                    ),
                ),
            }
        "#]],
    );
}

impl ConstantBackoff {
    pub fn builder(self) -> backon::ConstantBuilder {
        let Self {
            delay,
            max_times,
            jitter,
        } = self;
        backon::ConstantBuilder::new()
            .tap_opt(delay, |b, v| b.with_delay(v))
            .tap_opt(max_times, |b, v| match v {
                BackoffMaxTimes::False => b.without_max_times(),
                BackoffMaxTimes::MaxTimes(m) => b.with_max_times(m),
            })
            .tap_opt(jitter, |b, v| match v {
                BackoffJitter::True => b.with_jitter(),
                BackoffJitter::Seeded(s) => b.with_jitter().with_jitter_seed(s),
            })
    }
}

serde! {
    #[derive(Default)]
    pub struct ExponentialBackoff {
        pub jitter: Option<BackoffJitter>,
        pub factor: Option<f32>,
        #[serde_as(as = "Option<HumanDuration>")]
        pub min_delay: Option<Duration>,
        #[serde_as(as = "Option<HumanDuration>")]
        pub total_delay: Option<Duration>,
        pub max_delay: Option<BackoffMaxDelay>,
        pub max_times: Option<BackoffMaxTimes>,
    }
}

impl ExponentialBackoff {
    pub fn builder(self) -> backon::ExponentialBuilder {
        let Self {
            jitter,
            factor,
            min_delay,
            total_delay,
            max_times,
            max_delay,
        } = self;
        backon::ExponentialBuilder::new()
            .tap_opt(jitter, |b, v| match v {
                BackoffJitter::True => b.with_jitter(),
                BackoffJitter::Seeded(u) => b.with_jitter().with_jitter_seed(u),
            })
            .tap_opt(factor, |b, v| b.with_factor(v))
            .tap_opt(min_delay, |b, v| b.with_min_delay(v))
            .tap_opt(total_delay, |b, v| b.with_total_delay(Some(v)))
            .tap_opt(max_delay, |b, v| match v {
                BackoffMaxDelay::False => b.without_max_delay(),
                BackoffMaxDelay::MaxDelay(d) => b.with_max_delay(d),
            })
            .tap_opt(max_times, |b, v| match v {
                BackoffMaxTimes::False => b.without_max_times(),
                BackoffMaxTimes::MaxTimes(u) => b.with_max_times(u),
            })
    }
}

serde! {
    #[derive(Default)]
    pub struct FibonacciBackoff {
        pub jitter: Option<BackoffJitter>,
        #[serde_as(as = "Option<HumanDuration>")]
        pub min_delay: Option<Duration>,
        pub max_delay: Option<BackoffMaxDelay>,
        pub max_times: Option<BackoffMaxTimes>,
    }
}

impl FibonacciBackoff {
    pub fn builder(self) -> backon::FibonacciBuilder {
        let Self {
            jitter,
            min_delay,
            max_delay,
            max_times,
        } = self;
        backon::FibonacciBuilder::new()
            .tap_opt(jitter, |b, v| match v {
                BackoffJitter::True => b.with_jitter(),
                BackoffJitter::Seeded(u) => b.with_jitter().with_jitter_seed(u),
            })
            .tap_opt(min_delay, |b, v| b.with_min_delay(v))
            .tap_opt(max_delay, |b, v| match v {
                BackoffMaxDelay::False => b.without_max_delay(),
                BackoffMaxDelay::MaxDelay(d) => b.with_max_delay(d),
            })
            .tap_opt(max_times, |b, v| match v {
                BackoffMaxTimes::False => b.without_max_times(),
                BackoffMaxTimes::MaxTimes(u) => b.with_max_times(u),
            })
    }
}

serde! {
    #[serde(from = "_BackoffMaxDelay", into = "_BackoffMaxDelay")]
    #[schemars(with = "_BackoffMaxDelay")]
    pub enum BackoffMaxDelay {
        False,
        MaxDelay(Duration),
    }

    #[serde(untagged)]
    enum _BackoffMaxDelay {
        False(False),
        MaxDelay (
            #[serde_as(as = "HumanDuration")]
            Duration
        )
    }
}

impl From<_BackoffMaxDelay> for BackoffMaxDelay {
    fn from(value: _BackoffMaxDelay) -> Self {
        match value {
            _BackoffMaxDelay::False(False) => Self::False,
            _BackoffMaxDelay::MaxDelay(max_delay) => Self::MaxDelay(max_delay),
        }
    }
}

impl From<BackoffMaxDelay> for _BackoffMaxDelay {
    fn from(value: BackoffMaxDelay) -> Self {
        match value {
            BackoffMaxDelay::False => Self::False(False),
            BackoffMaxDelay::MaxDelay(max_delay) => Self::MaxDelay(max_delay),
        }
    }
}

serde! {
    #[serde(from = "_BackoffMaxTimes", into = "_BackoffMaxTimes")]
    #[schemars(with = "_BackoffMaxTimes")]
    pub enum BackoffMaxTimes {
        False,
        MaxTimes(usize),
    }

    #[serde(untagged)]
    enum _BackoffMaxTimes {
        MaxTimes(usize),
        False(False),
    }
}

impl From<_BackoffMaxTimes> for BackoffMaxTimes {
    fn from(value: _BackoffMaxTimes) -> Self {
        match value {
            _BackoffMaxTimes::False(False) => Self::False,
            _BackoffMaxTimes::MaxTimes(max_times) => Self::MaxTimes(max_times),
        }
    }
}
impl From<BackoffMaxTimes> for _BackoffMaxTimes {
    fn from(value: BackoffMaxTimes) -> Self {
        match value {
            BackoffMaxTimes::False => Self::False(False),
            BackoffMaxTimes::MaxTimes(max_times) => Self::MaxTimes(max_times),
        }
    }
}

serde! {
    #[serde(from = "_BackoffJitter", into = "_BackoffJitter")]
    #[schemars(with = "_BackoffJitter")]
    pub enum BackoffJitter {
        True,
        Seeded(u64),
    }

    #[serde(untagged)]
    enum _BackoffJitter {
        True(True),
        Seeded { seeded: u64 },
    }
}

impl From<_BackoffJitter> for BackoffJitter {
    fn from(value: _BackoffJitter) -> Self {
        match value {
            _BackoffJitter::True(True) => Self::True,
            _BackoffJitter::Seeded { seeded } => Self::Seeded(seeded),
        }
    }
}

impl From<BackoffJitter> for _BackoffJitter {
    fn from(value: BackoffJitter) -> Self {
        match value {
            BackoffJitter::True => _BackoffJitter::True(True),
            BackoffJitter::Seeded(seeded) => _BackoffJitter::Seeded { seeded },
        }
    }
}
