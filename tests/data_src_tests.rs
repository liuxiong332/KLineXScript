const MA_SCRIPT: &str = "
N = 5
ma = close
for i = 1 to N
    ma := ma + close[i] 
print(ma)
";
