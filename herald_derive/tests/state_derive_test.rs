use ::herald::prelude::*;

#[herald]
struct NormalGeneric<'a, T: Clone + PartialEq> {
    with_lifetime: &'a str,
    generic_type: T,
    #[herald(skip)]
    _without_state: f32,
}

#[test]
fn test_normal_generic() {
    let _normal = NormalGeneric {
        with_lifetime: "hi herald",
        generic_type: 0,
        _without_state: 1.0,
        _herald_info: HeraldInfo::new(),
    };
    let state = NormalGenericStates::<u32>::default();
    assert_eq!(state.with_lifetime, None);
    assert_eq!(state.generic_type, None);
}

#[herald]
struct GenericInPath<T>
where
    T: 'static + Clone + PartialEq + Send + Sync,
{
    vec_generic: Vec<T>,
}

#[test]
fn test_generic_in_path() {
    let _vec = GenericInPath {
        vec_generic: vec![1],
        _herald_info: HeraldInfo::new(),
    };
    let state = GenericInPathStates::<i32>::default();
    assert_eq!(state.vec_generic, None);
}

#[test]
fn getter_setter_implemented() {
    let mut vec = GenericInPath {
        vec_generic: vec![1],
        _herald_info: HeraldInfo::new(),
    };

    assert_eq!(vec.get_vec_generic(), &vec![1]);
    vec.set_vec_generic(vec![2]);
    assert_eq!(vec.get_vec_generic(), &vec![2]);
}

#[test]
#[should_panic(expected = "change work fine!")]
fn setter_should_trigger_change_event() {
    let mut vec = GenericInPath {
        vec_generic: vec![1],
        _herald_info: HeraldInfo::new(),
    };

    vec._herald_info
        .immediate_changes_events()
        .subscribe_change(|mut chg: ChangeEvent<'_, _, GenericInPathChanges<_>>| {
            let chg = chg.changes().vec_generic.as_ref().unwrap();
            assert_eq!(chg.before, vec![1]);
            assert_eq!(chg.after, vec![2]);
            panic!("change work fine!");
        });

    vec.set_vec_generic(vec![2]);
}
