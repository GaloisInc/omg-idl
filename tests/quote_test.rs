#[macro_use]
extern crate quote;

#[test]
fn main() {
    let variants = vec!["Foo", "Bar", "Baz"]
        .into_iter()
        .map(|s| quote::Ident::new(s));
    let tokens = quote! {
        enum Quux { #(#variants),* }
    };
    println!("{}", tokens.to_string());
}
