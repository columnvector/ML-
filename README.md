### input에 대해 n으로 나누었을때,나머지가 v인 부분집합과 그위치, 합 계산  

    comb <- function(input,n,v){
    idx <- which(input%%n==v)
    comb_idx <- input[idx]
    value <- sum(input[input%%n==v])
    cat(input, "을", n, "으로 나눈 나머지가", v, "인 집합은", comb_idx, "\n")
    cat(input, "을", n, "으로 나눈 나머지가", v, "인 집합의 위치는", idx, "\n")
    cat(input, "을", n, "으로 나눈 나머지가", v, "인 집합의 합은", value, "\n")
    }


    input <- c(55,38,5,60,32,17,80,10,67,98,21,87,9,49,39)
    comb(input,4,1)
    
### pi 근사 (10000)
 
     pi <- function (n){
            tmp = rep(0,n)
              for(i in 1:n){
                tmp[i] = 4 * (1/ (2*i-1) * -(-1)^i)
              }
              sum(tmp)
              }

    print(pi(10000))
