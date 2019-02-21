## The below is code to perform a t.test based on one or two populations against any specified mean with any specified alternative
## Alternative set to two.sided as default
## function will run single variable t test if y is not specificed
## THe function returns pvalue, degrees of freedom and t statistic 
my.t.test <- function(x,y = 0,c0=0, alternative = "two.sided"){
  
  d1 <- na.omit(x)
  d2 <- na.omit(y) 
  
  n1 <- length(d1)
  n2<- length(d2)
  t_stat <- 0
  t0<- 0
  n<- length(d1)
  df<- 0
  ### wrap everything in giant if statment to decifer if one sample or two sample
  if(n2 > 1){
    v_test<- var.test(d1,d2)
    sp <-0 
    df<- 0
    t_stat <- 0
    d1_var <- var(d1)
    d2_var <- var(d2)
    if(v_test$p.value > 0.05){
      numerator <- (n1-1)*d1_var + (n2-1)*d2_var
      denum <- (n1+n2 - 2)
      sp <- numerator/denum
      df <- (n1+n2 - 2)
      t_stat<- (mean(d1) - mean(d2) - c0) /( sqrt(sp)*sqrt((1/n1)+(1/n2)) )
      p_value <- pt(q = t_stat, df = df, lower.tail = T)
      
    } else {numerator <- (d1_var/n1 + d2_var/n2)^2 
    denum <- ((d1_var/n1)^2)/(n1-1)  + ((d2_var/n2)^2)/(n2-1)
    df <- numerator/denum
    t_stat<- (mean(d1) - mean(d2) - c0) /( sqrt(d1_var/n1 + d2_var/n2) )
    p_value <- pt(q = t_stat, df = df, lower.tail = T)
    }
  } else {
    x <- na.omit(x)
    n<- length(x)
    t0 <- sqrt(n)*(mean(x) - c0)/sd(x)
    t_stat<- t0
    p_value <- pt(abs(t0), df = n-1,lower.tail = F)
  }
  
  
  
  if(alternative == "two.sided"){
    p_value <-  2*p_value
  } else if(alternative == "greater") {
    t0 <- sqrt(n)*(mean(x) - c0)/sd(x)
    p_value <- pt((t0), df = n-1,lower.tail = F)
    p_value <-p_value} 
  else if(alternative == "less") { t0 <- sqrt(n)*(c0 - mean(x))/sd(x)
  p_value <- pt((t_stat), df = n-1,lower.tail = T)
  p_value <- p_value }
  
  
  return(list(p.value = p_value,t.stat = t_stat,df = df))
}
