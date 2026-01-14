use crate::*;

serde! {
    #[derive(Default)]
    pub struct ConstantBackoff {
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub delay: Option<Duration>,
        pub max_times: Option<FalseOr<usize>>,
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
                FalseOr::False => b.without_max_times(),
                FalseOr::Or(m) => b.with_max_times(m),
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
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub min_delay: Option<Duration>,
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub total_delay: Option<Duration>,
        #[serde_as(as = "Option<FalseOrWith<AsHumanDuration, Duration>>")]
        pub max_delay: Option<FalseOr<Duration>>,
        pub max_times: Option<FalseOr<usize>>,
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
                FalseOr::False => b.without_max_delay(),
                FalseOr::Or(d) => b.with_max_delay(d),
            })
            .tap_opt(max_times, |b, v| match v {
                FalseOr::False => b.without_max_times(),
                FalseOr::Or(u) => b.with_max_times(u),
            })
    }
}

serde! {
    #[derive(Default)]
    pub struct FibonacciBackoff {
        pub jitter: Option<BackoffJitter>,
        #[serde_as(as = "Option<AsHumanDuration>")]
        pub min_delay: Option<Duration>,
        #[serde_as(as = "Option<FalseOrWith<AsHumanDuration, Duration>>")]
        pub max_delay: Option<FalseOr<Duration>>,
        pub max_times: Option<FalseOr<usize>>,
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
                FalseOr::False => b.without_max_delay(),
                FalseOr::Or(d) => b.with_max_delay(d),
            })
            .tap_opt(max_times, |b, v| match v {
                FalseOr::False => b.without_max_times(),
                FalseOr::Or(u) => b.with_max_times(u),
            })
    }
}

serde! {
    #[serde(from = "Untagged<True, Seeded>", into = "Untagged<True, Seeded>")]
    #[schemars(with = "Untagged<True, Seeded>")]
    pub enum BackoffJitter {
        True,
        Seeded(u64),
    }

    struct Seeded { seeded: u64 }
}

convert_enum! {
    BackoffJitter = Untagged<True, Seeded> {
        [BackoffJitter::Seeded(seeded)] = [Untagged::Right(Seeded { seeded })]
        [BackoffJitter::True]           = [Untagged::Left(True)]
    }
}
