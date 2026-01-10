use std::{num::NonZero, time::Duration};

use crate::*;

serde! {
    #[derive(Default)]
    pub struct TokioRuntime {
        pub flavor: TokioRuntimeFlavor,
        pub worker_threads: Option<NonZero<usize>>,
        pub max_blocking_threads: Option<NonZero<usize>>,
        pub thread_name: Option<String>,
        pub thread_stack_size: Option<usize>,
        #[serde_as(as = "Option<HumanDuration>")]
        pub thread_keepalive: Option<Duration>,
        pub global_queue_interval: Option<NonZero<u32>>,
        pub event_interval: Option<u32>,
        pub enable_io: Option<bool>,
        pub max_io_events_per_tick: Option<usize>,
        pub enable_time: Option<bool>,
    }

    #[derive(Default)]
    pub enum TokioRuntimeFlavor {
        CurrentThread,
        #[default]
        MultiThread,
    }
}

impl TokioRuntime {
    pub fn builder(self) -> tokio::runtime::Builder {
        let Self {
            flavor,
            worker_threads,
            max_blocking_threads,
            thread_stack_size,
            thread_keepalive,
            global_queue_interval,
            event_interval,
            enable_io,
            max_io_events_per_tick,
            enable_time,
            thread_name,
        } = self;
        let mut b = match flavor {
            TokioRuntimeFlavor::CurrentThread => tokio::runtime::Builder::new_current_thread(),
            TokioRuntimeFlavor::MultiThread => tokio::runtime::Builder::new_multi_thread(),
        };
        (&mut b)
            .tap_opt(worker_threads, |b, v| b.worker_threads(v.get()))
            .tap_opt(max_blocking_threads, |b, v| b.max_blocking_threads(v.get()))
            .tap_opt(thread_name, |b, v| b.thread_name(v))
            .tap_opt(thread_stack_size, |b, v| b.thread_stack_size(v))
            .tap_opt(thread_keepalive, |b, v| b.thread_keep_alive(v))
            .tap_opt(global_queue_interval, |b, v| {
                b.global_queue_interval(v.get())
            })
            .tap_opt(event_interval, |b, v| b.event_interval(v))
            .tap_opt(max_io_events_per_tick, |b, v| b.max_io_events_per_tick(v));
        if let Some(true) = enable_io {
            b.enable_io();
        }
        if let Some(true) = enable_time {
            b.enable_time();
        }
        b
    }
}
