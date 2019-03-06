978. 最长湍流子数组

1. check the signal of (Ai-A_{i-1}) * (A(i)-A(i+1)) is +

2. count the length of consecutive +

3. special case for length 1 and 2 

4. (Ai-A_{i-1}) * (A(i)-A(i+1)) may be out of bound, replace with sign(Ai-A_{i-1}) * sign(A(i)-A(i+1)) 
