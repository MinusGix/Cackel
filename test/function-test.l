func main (argc : u32, argv: u64) -> i64 {
    return getValue(2) + 6; // 5 + 2 + 6 = 13
}

func getValue (val : u64) -> i64 {
    return 5 + val;
}