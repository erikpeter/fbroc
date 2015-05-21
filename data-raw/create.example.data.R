set.seed(123)

true.class <- rep(c(FALSE, TRUE), each = 80)
pred1 <- rnorm(160) + 2 * true.class
pred2 <- pred1
pred2[53:55] <- c(6, 7, 8)
pred3 <- c(rep(1:8, each = 10), rep(7:14, each = 10))
pred4 <- pred3
pred4[seq(10, 80, by = 10)] <- 15

roc.examples <- data.frame(True.Class = true.class, Cont.Pred = pred1, Cont.Pred.Outlier = pred2,
                           Disc.Pred = pred3, Disc.Pred.Outlier = pred4) 
devtools::use_data(roc.examples)
