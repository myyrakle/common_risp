use proc_macro::TokenStream;

#[proc_macro]
pub fn compile(item: TokenStream) -> TokenStream {
    item
}
