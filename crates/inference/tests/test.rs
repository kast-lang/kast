use kast_inference::*;

#[test]
fn test_three_ints() -> eyre::Result<()> {
    #[derive(Debug, Clone, PartialEq, Same)]
    struct Int(i32);

    impl Inferrable for Int {
        fn infer_as_subset_of(&self, superset: &Self) -> eyre::Result<()> {
            // superset.constrain(self)?;
            if self.0 != superset.0 {
                eyre::bail!("{self:?} != {superset:?}");
            }
            Ok(())
        }
        fn union(a: Self, b: Self) -> eyre::Result<Self> {
            if a.0 != b.0 {
                eyre::bail!("{a:?} != {b:?}")
            }
            Ok(a)
        }
        fn intersect(a: Self, b: Self) -> eyre::Result<Self> {
            if a.0 != b.0 {
                eyre::bail!("{a:?} != {b:?}")
            }
            Ok(a)
        }
    }

    let a = Var::<Int>::new_not_inferred("a");
    let b = Var::new_not_inferred("b");
    let c = Var::new_not_inferred("c");
    a.infer_as(&b)?;
    b.infer_as(&c)?;
    c.infer_as(Int(123))?;

    assert_eq!(a.inferred()?, Int(123));
    assert_eq!(b.inferred()?, Int(123));
    assert_eq!(c.inferred()?, Int(123));

    Ok(())
}

#[test]
fn test_subtype() -> eyre::Result<()> {
    #[derive(Debug, Clone, PartialEq, Same)]
    struct IntLessThan(i32);

    impl Inferrable for IntLessThan {
        fn infer_as_subset_of(&self, superset: &Self) -> eyre::Result<()> {
            if self.0 <= superset.0 {
                Ok(())
            } else {
                eyre::bail!("failed check {self:?} <= {superset:?}");
            }
        }
        fn union(Self(a): Self, Self(b): Self) -> eyre::Result<Self> {
            Ok(Self(std::cmp::max(a, b)))
        }
        fn intersect(Self(a): Self, Self(b): Self) -> eyre::Result<Self> {
            Ok(Self(std::cmp::min(a, b)))
        }
    }

    // let d :: int{<456}
    // let c :: int{<123}
    // let b = c; # typeof c is subset of typeof b
    // let mut a = b;
    // a = d;

    let a = Var::<IntLessThan>::new_not_inferred("a");
    let b = Var::new_not_inferred("b");
    let c = Var::new_not_inferred("c");
    let d = Var::new_not_inferred("d");
    a.infer_as_superset_of(&b)?;
    b.infer_as_superset_of(&c)?;
    c.infer_as(IntLessThan(123))?;
    a.infer_as_superset_of(&d)?;
    d.infer_as(IntLessThan(456))?;

    assert_eq!(a.inferred()?, IntLessThan(456));
    assert_eq!(b.inferred()?, IntLessThan(123));
    assert_eq!(c.inferred()?, IntLessThan(123));
    assert_eq!(d.inferred()?, IntLessThan(456));

    Ok(())
}

#[test]
fn test_type_set() -> eyre::Result<()> {
    #[derive(Debug, Clone, PartialEq, Eq)]
    struct Type(std::collections::BTreeSet<String>);

    impl Type {
        fn from_iter<S: AsRef<str>>(iter: impl IntoIterator<Item = S>) -> Self {
            Type(iter.into_iter().map(|s| s.as_ref().to_owned()).collect())
        }
        fn singleton(s: &str) -> Self {
            Self::from_iter([s])
        }
    }

    impl Same for Type {
        fn is_same(&self, other: &Self) -> bool {
            self.0 == other.0
        }
        fn hash(&self, state: &mut impl std::hash::Hasher) {
            std::hash::Hash::hash(&self.0, state)
        }
    }

    impl Inferrable for Type {
        fn infer_as_subset_of(&self, superset: &Self) -> eyre::Result<()> {
            if self.0.is_subset(&superset.0) {
                Ok(())
            } else {
                eyre::bail!("failed check {self:?} <= {superset:?}");
            }
        }
        fn union(Self(a): Self, Self(b): Self) -> eyre::Result<Self> {
            Ok(Self(a.union(&b).cloned().collect()))
        }
        fn intersect(Self(a): Self, Self(b): Self) -> eyre::Result<Self> {
            Ok(Self(a.intersection(&b).cloned().collect()))
        }
    }

    // let x = "hello"; # string
    // x = 123; # number
    // x :: string | number

    let x = Var::<Type>::new_not_inferred("x");
    x.infer_as_superset_of(Type::singleton("string"))?;
    x.infer_as_superset_of(Type::singleton("number"))?;
    assert_eq!(x.inferred()?, Type::from_iter(["string", "number"]));

    Ok(())
}
