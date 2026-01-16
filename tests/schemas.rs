macro_rules! test {
    ($($ident:ident: $ty:ty);* $(;)?) => {$(
        #[test]
        #[allow(non_snake_case)]
        fn $ident() {
            ensuring_parent_dir(::expect_test::expect_file![
                ::core::concat!("schemas/", ::core::stringify!($ty), ".schema.json")
            ]).assert_eq(
                &::std::format!(
                    "{:#}",
                    ::schemars::schema_for!($ty).as_value()
                )
            );
        }
    )*};
}

fn ensuring_parent_dir(e: expect_test::ExpectFile) -> expect_test::ExpectFile {
    std::fs::create_dir_all(e.path.parent().unwrap()).unwrap();
    e
}

test! {
    reqwest_client: konfig::reqwest::Client;
}
