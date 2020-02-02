#![recursion_limit = "256"]
extern crate proc_macro;
extern crate proc_macro2;

use crate::proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_str, punctuated::Punctuated, Attribute, Field, Fields,
    GenericArgument, GenericParam, Generics, Ident, ItemStruct, Meta, NestedMeta, PathArguments,
    Type,
};

const CISTERN_LIFETIME: &'static str = "'herald";
const SKIP_META: &'static str = "skip";
const RECURSION_META: &'static str = "recursion";

#[proc_macro_attribute]
pub fn herald(_attr: TokenStream, tokens: TokenStream) -> TokenStream {
    let mut stt = parse_macro_input!(tokens as ItemStruct);
    let code_gen = CodeGen::new(&mut stt);
    let herald_def = code_gen.gen_herald_def_tokens();
    let state_def = code_gen.gen_state_tokens();
    let herald_impl = code_gen.gen_herald_impl_tokens();
    let fields_update_impl = code_gen.gen_fields_update();

    let tokens = quote! {
         #herald_def
         #state_def

         #herald_impl

         #fields_update_impl
    };

    tokens.into()
}

struct CodeGen<'a> {
    stt: &'a ItemStruct,
    states: Vec<Field>,
}

macro_rules! format_ident {
    ($fmt: tt, $id: expr) => {{
        let state_name = format!($fmt, $id);
        Ident::new(&state_name, $id.span())
    }};
}

impl<'a> CodeGen<'a> {
    fn new(stt: &'a mut ItemStruct) -> CodeGen<'a> {
        let states = CodeGen::state_fields(stt);
        let lifetime: GenericParam = parse_str(CISTERN_LIFETIME).unwrap();
        stt.generics.params.push(lifetime.clone());
        assert!(
            states.len() > 0,
            format!(
                "There isn't state in {}, needn't derive Herald, maybe you want Ident?",
                stt.ident
            )
        );
        if let Fields::Named(ref mut fds) = stt.fields {
            fds.named.iter_mut().for_each(|f| {
                f.attrs.retain(|attr| !is_herald_attr(attr));
            });
        }
        let gen = CodeGen { stt, states };

        gen
    }

    fn gen_state_tokens(&self) -> impl ToTokens {
        let fields = self.map_states(|f| {
            let f_name = &f.ident;
            let ty = &f.ty;
            quote! { pub #f_name: Option<#ty>, }
        });

        let change_fields = self.map_states(|f| {
            let f_name = &f.ident;
            let ty = &f.ty;
            quote! { pub #f_name: Option<Change<#ty>>, }
        });

        let init = self.map_states(|f| {
            let f_name = &f.ident;
            quote! { #f_name: None, }
        });
        let change_init = init.clone();

        let state_type_name = self.state_ident();
        let state_change = self.state_change_ident();

        let state_generic = self.states_generics();
        let (state_impl_generic, state_ty_generics, state_where_clause) =
            state_generic.split_for_impl();
        quote! {
            #[derive(Clone)]
            struct #state_type_name #state_ty_generics #state_where_clause{
                #(#fields)*
            }

            #[derive(Clone)]
            struct #state_change #state_ty_generics #state_where_clause{
                #(#change_fields)*
            }

            impl #state_impl_generic Default for #state_type_name #state_ty_generics #state_where_clause{
                fn default() -> #state_type_name #state_ty_generics {
                    #state_type_name {
                        #(#init)*
                    }
                }
            }

             impl #state_impl_generic  Default for #state_change #state_ty_generics #state_where_clause{
                fn default() -> #state_change #state_ty_generics {
                    #state_change {
                        #(#change_init)*
                    }
                }
            }
        }
    }
    fn gen_herald_def_tokens(&self) -> impl ToTokens {
        let name = &self.stt.ident;
        let vis = &self.stt.vis;
        let fields = self.stt.fields.iter();
        let lifetime: GenericParam = parse_str(CISTERN_LIFETIME).unwrap();
        let (_, ty_generics, where_clause) = self.stt.generics.split_for_impl();

        let state_generic = self.states_generics();
        let (_, generics, _) = state_generic.split_for_impl();
        let state_change = self.state_change_ident();
        let change_type = quote! { #state_change #generics };

        quote! {
            #vis struct #name #ty_generics #where_clause {
                #(#fields ,)*
                _herald_info: HeraldInfo<#lifetime, #name #ty_generics, #change_type, ()>
            }
        }
    }

    fn gen_set_field_expr(
        batched: impl ToTokens,
        name: &Ident,
        v: impl ToTokens,
    ) -> proc_macro2::TokenStream {
        quote! {
            if self.#name != #v {
                let before = std::mem::replace(&mut self.#name, #v);
                let after = self.#name.clone();
                if let Some(ref mut change) = #batched.#name {
                    change.after = after.clone();
                } else {
                    #batched.#name = Some(Change { before: before.clone(), after: after.clone() });
                }
                Some(Change { before, after })
            } else {
                None
            }
        }
    }

    fn gen_herald_impl_tokens(&self) -> impl ToTokens {
        let name = &self.stt.ident;
        let state_name = self.state_ident();
        let state_change = self.state_change_ident();

        let c_life: GenericParam = parse_str(CISTERN_LIFETIME).unwrap();
        let (impl_generics, ty_generics, where_clause) = self.stt.generics.split_for_impl();
        let state_generic = self.states_generics();
        let (_, state_ty_generics, _) = state_generic.split_for_impl();

        let set_changes = self.map_states_ident(|name| {
            let change = Self::gen_set_field_expr(quote! {batched}, name, quote! {v});
            quote! {
                if let Some(v) = states.#name {
                    let change = #change;
                    changed = changed || change.is_some();
                    chgs.#name = change;
                }
            }
        });

        let ty_states = quote! {#state_name #state_ty_generics};
        let ty_changes = quote! {#state_change #state_ty_generics};
        quote! {
            impl #impl_generics #name #ty_generics #where_clause{
                fn update_states(&mut self, states: #ty_states) {
                    let mut chgs = #state_change::default();
                    let batched = self._herald_info.batched();
                    let mut changed = false;
                    #(#set_changes)*
                    if changed {
                        self.send_event(Event::ImmediateChange(chgs));
                    }
                }

                fn send_event(&mut self, event: Event<#ty_changes,()>){
                    unsafe {
                        let p_mut: *mut HeraldInfo<'_, _, _, _> = &mut self._herald_info;
                        let events = &mut *p_mut;
                        events.send_event(self, event);
                    }
                }
            }

            impl #impl_generics Herald<#c_life> for #name #ty_generics #where_clause{
                type StateChange = #ty_changes;
                type UserEvent = ();
                #[inline]
                fn herald(&mut self) -> &mut HeraldInfo<#c_life, Self, Self::StateChange, Self::UserEvent> {
                    &mut self._herald_info
                }
            }
            impl #impl_generics Ident for #name #ty_generics #where_clause{}

        }
    }

    fn gen_fields_update(&self) -> impl ToTokens {
        let name = &self.stt.ident;
        let (impl_generics, ty_generics, where_clause) = self.stt.generics.split_for_impl();
        let get_set_impl = self.map_states(|f| {
            let f_name = f.ident.as_ref().unwrap();
            let ty = &f.ty;

            let getter = format_ident!("get_{}", f_name);
            let setter = format_ident!("set_{}", f_name);
            let state_change = self.state_change_ident();
            let set_field = Self::gen_set_field_expr(quote! {batched}, f_name, quote! {value});
            let tokens = quote! {
                pub fn #getter(&self) -> &#ty{
                    &self.#f_name
                }

                pub fn #setter(&mut self, value: #ty)-> &mut Self{
                    if self.#f_name != value {
                        let batched = self._herald_info.batched();
                        let change = #set_field;
                        if change.is_some() {
                            let mut chgs = #state_change::default();
                            chgs.#f_name = change;
                            self.send_event(Event::ImmediateChange(chgs));
                        }
                    }
                    self
                }
            };
            tokens
        });

        quote! {
            impl #impl_generics #name #ty_generics #where_clause{
                #(#get_set_impl)*
            }
        }
    }

    fn state_ident(&self) -> Ident {
        format_ident!("{}States", self.stt.ident)
    }

    fn state_change_ident(&self) -> Ident {
        format_ident!("{}Changes", self.stt.ident)
    }

    fn state_fields(stt: &ItemStruct) -> Vec<Field> {
        match stt.fields {
            Fields::Unnamed(_) => panic!("Herald not support tuple struct"),
            Fields::Unit => panic!("Herald not support tuple struct"),
            Fields::Named(ref fds) => fds
                .named
                .iter()
                .filter_map(|f| {
                    if !field_has_meta(f, SKIP_META) {
                        Some(f.clone())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>(),
        }
    }

    fn states_generics(&self) -> Generics {
        self.generics_pick(|ident| {
            self.states
                .iter()
                .any(|f| Self::type_contain(&f.ty, ident.clone()))
        })
    }

    fn generics_pick(&self, predicate: impl Fn(&Ident) -> bool) -> Generics {
        let mut generics = self.stt.generics.clone();
        generics.params = generics
            .params
            .iter()
            .map(|p| p.clone())
            .filter(|p| {
                let ident = match p {
                    GenericParam::Type(t) => Some(t.ident.clone()),
                    GenericParam::Lifetime(l) => Some(l.lifetime.ident.clone()),
                    GenericParam::Const(_) => None,
                };
                if let Some(ident) = ident {
                    predicate(&ident)
                } else {
                    false
                }
            })
            .collect::<Punctuated<_, syn::token::Comma>>();
        generics
    }

    fn type_contain(ty: &Type, generic_ident: Ident) -> bool {
        let res = match ty {
            Type::Reference(ty_ref) => ty_ref
                .lifetime
                .as_ref()
                .map(|l| l.ident == generic_ident)
                .unwrap_or(false),
            Type::Slice(_slice) => unimplemented!("Slice Type not support by Herald yet."),
            Type::Array(_arr) => unimplemented!("Array type not support by Herald yet"),
            Type::Ptr(_ptr) => unimplemented!("Ptr type not support by Herald yet"),
            Type::BareFn(_bare_fn) => unimplemented!("BareFn type not support by Herald yet"),
            Type::Never(_never) => unimplemented!("Never type not support by Herald yet"),
            Type::Tuple(_tuple) => unimplemented!("Tuple type not support by Herald yet"),
            Type::Path(path) => path.path.segments.iter().any(|seg| {
                seg.ident == generic_ident
                    || match seg.arguments {
                        PathArguments::AngleBracketed(ref args) => {
                            args.args.iter().any(|arg| match arg {
                                GenericArgument::Lifetime(ref lifetime) => {
                                    lifetime.ident == generic_ident
                                }
                                GenericArgument::Type(ref ty) => {
                                    Self::type_contain(ty, generic_ident.clone())
                                }
                                _ => false,
                            })
                        }
                        PathArguments::Parenthesized(ref func) => {
                            let input_contain = func
                                .inputs
                                .iter()
                                .any(|ty| Self::type_contain(ty, generic_ident.clone()));
                            let mut return_contain = false;
                            if let syn::ReturnType::Type(_, ref output) = func.output {
                                return_contain =
                                    Self::type_contain(output.as_ref(), generic_ident.clone());
                            }
                            input_contain || return_contain
                        }
                        _ => false,
                    }
            }),
            Type::TraitObject(_trait_obj) => {
                unimplemented!("TraitObject type not support by Herald yet")
            }
            Type::ImplTrait(_impl_trait) => {
                unimplemented!("ImplTrait type not support by Herald yet")
            }
            Type::Paren(_paren) => unimplemented!("Paren type not support by Herald yet"),
            Type::Group(_group) => unimplemented!("Group type not support by Herald yet"),
            Type::Infer(_infer) => unimplemented!("Infer type not support by Herald yet"),
            Type::Macro(_macro) => unimplemented!("Macro type not support by Herald yet"),
            Type::Verbatim(_verbatim) => unimplemented!("Verbatim type not support by Herald yet"),
            Type::__Nonexhaustive => unreachable!(),
        };
        res
    }

    fn map_states_ident(
        &self,
        map: impl Fn(&Ident) -> proc_macro2::TokenStream,
    ) -> Vec<proc_macro2::TokenStream> {
        self.map_states(|f| map(f.ident.as_ref().unwrap()))
    }

    fn map_states(
        &self,
        map: impl Fn(&Field) -> proc_macro2::TokenStream,
    ) -> Vec<proc_macro2::TokenStream> {
        self.states.iter().map(|f| map(f)).collect::<Vec<_>>()
    }
}

fn field_has_meta(field: &Field, name: &str) -> bool {
    field
        .attrs
        .iter()
        .filter(|attr| is_herald_attr(attr))
        .any(|attr| {
            if let Ok(ref meta) = attr.parse_meta() {
                meta_has_name(meta, name)
            } else {
                false
            }
        })
}

fn is_herald_attr(attr: &Attribute) -> bool {
    attr.path.segments.len() == 1 && attr.path.segments[0].ident == "herald"
}

fn meta_has_name(meta: &Meta, name: &str) -> bool {
    match meta {
        Meta::Path(w) => w.is_ident(name),
        Meta::List(l) => l.nested.iter().any(|m| {
            if let NestedMeta::Meta(ref meta) = m {
                meta_has_name(meta, name)
            } else {
                false
            }
        }),
        _ => false,
    }
}
