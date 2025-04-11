use kast::*;

// TODO: Make activation watermark on stream bigger
fn main() -> eyre::Result<()> {
    let mut kast = Kast::new()?;
    kast.register_fn("sum_all_ints", |v: Vec<i64>| -> i64 { v.into_iter().sum() });
    let result = kast.eval_str_as::<i64>("sum_all_ints (list[1, 2, 3, 4, 5])")?;
    assert_eq!(result, 15);
    Ok(())
}
