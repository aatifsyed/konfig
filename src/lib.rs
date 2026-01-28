use schemars::JsonSchema;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_with::{DeserializeAs, DisplayFromStr, Map, SerializeAs, schemars_1::JsonSchemaAs};
use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    fmt,
    marker::PhantomData,
    time::{Duration, Instant},
};

pub mod backon;
pub mod otlp;
pub mod regex;
pub mod reqwest;
pub mod tokio;
pub mod tracing;

macro_rules! serde {
    ($($item:item)*) => {
        $(
            #[::serde_with::skip_serializing_none]
            #[::serde_with::serde_as]
            #[derive(::serde::Serialize, ::serde::Deserialize, ::schemars::JsonSchema, Clone, Debug)]
            #[serde(deny_unknown_fields, rename_all = "kebab-case")]
            $item
        )*
    };
}
pub(crate) use serde;

macro_rules! convert_enum {
    ($(
        $left:ty = $right:ty $([$($generics:tt)*])? {
            $( [$($l:tt)*] = [$($r:tt)*] )*
        }
    )*) => {$(
        impl $(<$($generics)*>)? From<$left> for $right {
            fn from(value: $left) -> $right {
                match value {
                    $( $($l)* => $($r)* ),*
                }
            }
        }
        impl $(<$($generics)*>)? From<$right> for $left {
            fn from(value: $right) -> $left {
                match value {
                    $( $($r)* => $($l)* ),*
                }
            }
        }
    )*};
}
pub(crate) use convert_enum;

#[cfg(test)]
pub(crate) use {expect_test::expect, serde_json::json};

#[cfg(test)]
#[track_caller]
fn assert_round_trip<T: serde::de::DeserializeOwned + Serialize + std::fmt::Debug>(
    json: serde_json::Value,
    exp: expect_test::Expect,
) {
    let de = serde_path_to_error::deserialize::<_, T>(&json).expect("couldn't deserialize");
    let rt = serde_json::to_value(&de).expect("couldn't serialize");

    exp.assert_debug_eq(&de);
    assert_eq!(json, rt, "did not round-trip");
}

type BoxError = Box<dyn std::error::Error + Send + Sync + 'static>;

trait Ext: Sized {
    fn tap_opt<T>(self, opt: Option<T>, f: impl FnOnce(Self, T) -> Self) -> Self {
        match opt {
            Some(it) => f(self, it),
            None => self,
        }
    }
    fn try_tap_opt<T, E>(
        self,
        opt: Option<T>,
        f: impl FnOnce(Self, T) -> Result<Self, E>,
    ) -> Result<Self, E> {
        match opt {
            Some(it) => f(self, it),
            None => Ok(self),
        }
    }
}

impl<T> Ext for T {}

struct HumanTime;

impl SerializeAs<Duration> for HumanTime {
    fn serialize_as<S: Serializer>(this: &Duration, s: S) -> Result<S::Ok, S::Error> {
        s.collect_str(&humantime::format_duration(*this))
    }
}
impl<'de> DeserializeAs<'de, Duration> for HumanTime {
    fn deserialize_as<D: Deserializer<'de>>(d: D) -> Result<Duration, D::Error> {
        let s = String::deserialize(d)?;
        humantime::parse_duration(&s).map_err(|e| {
            struct Expected(humantime::DurationError);
            impl serde::de::Expected for Expected {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    let Self(e) = self;
                    f.write_fmt(format_args!("a human-readable duration ({e})"))
                }
            }
            serde::de::Error::invalid_value(serde::de::Unexpected::Str(&s), &Expected(e))
        })
    }
}

impl JsonSchemaAs<Duration> for HumanTime {
    fn schema_name() -> Cow<'static, str> {
        "HumanTime<Duration>".into()
    }
    fn json_schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "title": "HumanTime",
            "type": "string",
            "description": "a human-readable duration",
            "examples": [
                "1 day 3 hours"
            ],
        })
    }
    fn inline_schema() -> bool {
        false
    }
    fn schema_id() -> Cow<'static, str> {
        format!(
            "{}::{}",
            module_path!(),
            <Self as JsonSchemaAs<Duration>>::schema_name()
        )
        .into()
    }
}

serde! {
    #[serde(untagged, bound(
        serialize = "B: AsRef<[u8]>, S: Serialize",
        deserialize = "B: AsRef<[u8]> + From<Vec<u8>>, S: Deserialize<'de>"
    ))]
    enum OrHex<S, B> {
        Inline(S),
        Hex { #[serde_as(as = "serde_with::hex::Hex")] hex: B },
    }

    #[serde(untagged)]
    enum Untagged<L, R> {
        Left(L),
        Right(R),
    }

    #[serde(from = "Untagged<False, T>", into = "Untagged<False, T>")]
    #[serde(bound(serialize = "T: Clone + Serialize"))]
    #[schemars(with = "Untagged<False, T>")]
    pub enum FalseOr<T> {
        False,
        Or(T)
    }

    #[serde(from = "Untagged<True, T>", into = "Untagged<True, T>")]
    #[serde(bound(serialize = "T: Clone + Serialize"))]
    #[schemars(with = "Untagged<True, T>")]
    pub enum TrueOr<T> {
        True,
        Or(T)
    }

    #[serde(untagged, expecting = "a map of string to string, or a sequence of pairs of string")]
    enum _HeaderMap {
        Map(
            #[serde_as(as = "Map<DisplayFromStr, HeaderValue>")]
            Vec<(http::HeaderName, http::HeaderValue)>
        ),
        Seq(
            #[serde_as(as = "Vec<(DisplayFromStr, HeaderValue)>")]
            Vec<(http::HeaderName, http::HeaderValue)>
        )
    }

    #[derive(Default)]
    pub struct OpenOptions {
        pub create: Option<bool>,
        pub create_new: Option<bool>,
        pub append: Option<bool>,
        pub truncate: Option<bool>,
        pub read: Option<bool>,
        pub write: Option<bool>,
    }
}

impl OpenOptions {
    pub fn build(self) -> std::fs::OpenOptions {
        let Self {
            create,
            create_new,
            append,
            truncate,
            read,
            write,
        } = self;
        let mut o = std::fs::OpenOptions::new();
        (&mut o)
            .tap_opt(create, |b, v| b.create(v))
            .tap_opt(create_new, |b, v| b.create_new(v))
            .tap_opt(append, |b, v| b.append(v))
            .tap_opt(truncate, |b, v| b.truncate(v))
            .tap_opt(read, |b, v| b.read(v))
            .tap_opt(write, |b, v| b.write(v));
        o
    }
}

convert_enum! {
    FalseOr<T> = Untagged<False, T> [T] {
        [FalseOr::False] = [Untagged::Left(False)]
        [FalseOr::Or(it)] = [Untagged::Right(it)]
    }
    TrueOr<T> = Untagged<True, T> [T] {
        [TrueOr::True] = [Untagged::Left(True)]
        [TrueOr::Or(it)] = [Untagged::Right(it)]
    }
}

impl<T> FalseOr<T> {
    fn into_option(self) -> Option<T> {
        match self {
            FalseOr::False => None,
            FalseOr::Or(it) => Some(it),
        }
    }
}

impl<T> TrueOr<T> {
    fn into_option(self) -> Option<T> {
        match self {
            TrueOr::True => None,
            TrueOr::Or(it) => Some(it),
        }
    }
}

struct FalseOrWith<A, J>(PhantomData<(A, J)>);

impl<A: SerializeAs<T>, T, J> SerializeAs<FalseOr<T>> for FalseOrWith<A, J> {
    fn serialize_as<S: Serializer>(this: &FalseOr<T>, s: S) -> Result<S::Ok, S::Error> {
        match this {
            FalseOr::False => false.serialize(s),
            FalseOr::Or(it) => A::serialize_as(it, s),
        }
    }
}

#[derive(Deserialize, Serialize)]
enum Never {}

impl<'de, A: DeserializeAs<'de, T>, T, J> DeserializeAs<'de, FalseOr<T>> for FalseOrWith<A, J> {
    fn deserialize_as<D: Deserializer<'de>>(d: D) -> Result<FalseOr<T>, D::Error> {
        #[derive(Deserialize)]
        #[serde(bound = "A: DeserializeAs<'de, T>")]
        enum FalseOrWithUntagged<T, A> {
            False(False),
            Or(#[serde(with = "serde_with::As::<A>")] T),
            As { never: Never, _a: PhantomData<A> },
        }
        Ok(match FalseOrWithUntagged::<T, A>::deserialize(d)? {
            FalseOrWithUntagged::False(False) => FalseOr::False,
            FalseOrWithUntagged::Or(it) => FalseOr::Or(it),
            FalseOrWithUntagged::As { never, _a } => match never {},
        })
    }
}

impl<A: JsonSchemaAs<J>, T, J> JsonSchemaAs<T> for FalseOrWith<A, J> {
    fn schema_name() -> Cow<'static, str> {
        format!("FalseOr<{}>", A::schema_name()).into()
    }

    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "anyOf": [
                {
                    "const": false
                },
                A::json_schema(generator)
            ]
        })
    }

    fn schema_id() -> Cow<'static, str> {
        format!(
            "{}::{}",
            module_path!(),
            <Self as JsonSchemaAs<T>>::schema_name()
        )
        .into()
    }
}

struct HeaderValue;

impl SerializeAs<http::HeaderValue> for HeaderValue {
    fn serialize_as<S: Serializer>(this: &http::HeaderValue, s: S) -> Result<S::Ok, S::Error> {
        match this.to_str() {
            Ok(it) => OrHex::Inline(it),
            Err(_) => OrHex::Hex {
                hex: this.as_bytes(),
            },
        }
        .serialize(s)
    }
}

impl<'de> DeserializeAs<'de, http::HeaderValue> for HeaderValue {
    fn deserialize_as<D: Deserializer<'de>>(d: D) -> Result<http::HeaderValue, D::Error> {
        match OrHex::<String, Vec<_>>::deserialize(d)? {
            OrHex::Inline(it) => http::HeaderValue::try_from(it),
            OrHex::Hex { hex } => http::HeaderValue::try_from(hex),
        }
        .map_err(serde::de::Error::custom)
    }
}

impl JsonSchemaAs<http::HeaderValue> for HeaderValue {
    fn schema_name() -> Cow<'static, str> {
        "HeaderValue".into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        OrHex::<String, Vec<u8>>::json_schema(generator)
    }
    fn schema_id() -> Cow<'static, str> {
        format!("http::{}", Self::schema_name()).into()
    }
}

struct HeaderMap;

impl SerializeAs<http::HeaderMap> for HeaderMap {
    fn serialize_as<S: Serializer>(this: &http::HeaderMap, s: S) -> Result<S::Ok, S::Error> {
        _HeaderMap::Seq(this.iter().map(|(k, v)| (k.clone(), v.clone())).collect()).serialize(s)
    }
}

impl<'de> DeserializeAs<'de, http::HeaderMap> for HeaderMap {
    fn deserialize_as<D: Deserializer<'de>>(d: D) -> Result<http::HeaderMap, D::Error> {
        let (_HeaderMap::Map(v) | _HeaderMap::Seq(v)) = Deserialize::deserialize(d)?;
        Ok(http::HeaderMap::from_iter(v))
    }
}

impl JsonSchemaAs<http::HeaderMap> for HeaderMap {
    fn schema_name() -> Cow<'static, str> {
        "HeaderMap".into()
    }
    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        _HeaderMap::json_schema(generator)
    }
    fn schema_id() -> Cow<'static, str> {
        format!("http::{}", Self::schema_name()).into()
    }
}

struct BoundedFloat<const LO: i64, const HI: i64>;

impl<'de, const LO: i64, const HI: i64, T: Deserialize<'de> + Into<f64> + Clone>
    DeserializeAs<'de, T> for BoundedFloat<LO, HI>
{
    fn deserialize_as<D: Deserializer<'de>>(d: D) -> Result<T, D::Error> {
        let f = T::deserialize(d)?;
        let rng = (LO as f64)..=(HI as f64);
        match rng.contains(&f.clone().into()) {
            true => Ok(f),
            false => Err(serde::de::Error::invalid_value(
                serde::de::Unexpected::Float(f.into()),
                &&*format!("a float in the range {rng:?}"),
            )),
        }
    }
}

impl<const LO: i64, const HI: i64, T: Serialize> SerializeAs<T> for BoundedFloat<LO, HI> {
    fn serialize_as<S: Serializer>(this: &T, s: S) -> Result<S::Ok, S::Error> {
        this.serialize(s)
    }
}

impl<const LO: i64, const HI: i64, T> JsonSchemaAs<T> for BoundedFloat<LO, HI> {
    fn schema_name() -> Cow<'static, str> {
        format!("BoundedFloat<{LO}, {HI}>").into()
    }
    fn json_schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "type": "number",
            "minimum": LO,
            "maximum": HI,
        })
    }
    fn inline_schema() -> bool {
        true
    }
    fn schema_id() -> Cow<'static, str> {
        format!(
            "{}::{}",
            module_path!(),
            <Self as JsonSchemaAs<T>>::schema_name()
        )
        .into()
    }
}

trait ToUnexpected {
    fn to_unexpected(&self) -> serde::de::Unexpected<'_>;
}

impl<T: ToUnexpected + ?Sized> ToUnexpected for &T {
    fn to_unexpected(&self) -> serde::de::Unexpected<'_> {
        T::to_unexpected(self)
    }
}

impl ToUnexpected for bool {
    fn to_unexpected(&self) -> serde::de::Unexpected<'_> {
        serde::de::Unexpected::Bool(*self)
    }
}
impl ToUnexpected for String {
    fn to_unexpected(&self) -> serde::de::Unexpected<'_> {
        serde::de::Unexpected::Str(self)
    }
}
impl ToUnexpected for str {
    fn to_unexpected(&self) -> serde::de::Unexpected<'_> {
        serde::de::Unexpected::Str(self)
    }
}

macro_rules! literal {
    ($(
        $(#[$meta:meta])*
        $vis:vis struct $ident:ident($ty:ty = $val:expr);
    )*) => {$(
        $(#[$meta])*
        $vis struct $ident;

        impl Serialize for $ident {
            fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
                $val.serialize(s)
            }
        }
        impl<'de> Deserialize<'de> for $ident {
            fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
                let it = <$ty>::deserialize(d)?;
                match it == $val {
                    true => Ok(Self),
                    false => Err(serde::de::Error::invalid_value(ToUnexpected::to_unexpected(&it), &&*format!("{}", $val)))
                }
            }
        }
        impl JsonSchema for $ident {
            fn schema_name() -> Cow<'static, str> {
                format!("Literal<{:?}>", $val).into()
            }
            fn json_schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
                schemars::json_schema!({
                    "const": $val
                })
            }
            fn inline_schema() -> bool {
                true
            }
            fn schema_id() -> Cow<'static, str> {
                format!("{}::{}", module_path!(), Self::schema_name()).into()
            }
        }
    )*};
}

literal! {
    #[derive(Clone, Debug)]
    struct True(bool = true);
    #[derive(Clone, Debug)]
    struct False(bool = false);
}

fn is_empty<T: IsEmpty>(this: &T) -> bool {
    T::is_empty(this)
}

trait IsEmpty {
    fn is_empty(&self) -> bool;
}

impl IsEmpty for String {
    fn is_empty(&self) -> bool {
        String::is_empty(self)
    }
}

impl<T> IsEmpty for Vec<T> {
    fn is_empty(&self) -> bool {
        Vec::is_empty(self)
    }
}
impl<K, V> IsEmpty for BTreeMap<K, V> {
    fn is_empty(&self) -> bool {
        BTreeMap::is_empty(self)
    }
}

impl<K, V, S> IsEmpty for HashMap<K, V, S> {
    fn is_empty(&self) -> bool {
        HashMap::is_empty(self)
    }
}
