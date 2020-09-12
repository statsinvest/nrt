# NYSE: NRT
# North European Oil Royalty Trust
# 2020-09-12

set.seed(1337);

# individual risk-free rate
discount <- 0.0065;

# federal income bracket: $39,376 - $434,500
# net investment income tax: > $200,000
# state tax
# local tax
tax <- 0.15 + 0.038 + 0.088 + 0.039;

# number of simulation rounds
B <- 10000;

# net reserves at the start of 2020
reserves.2020 <- 6429.7;

# net sales in 2016, 2017, 2018, 2019
sales.past <- c(1392, 1330, 1066, 1215);

# according to 10-Q for 2020 Q1, Q2, and Q3, sales is down about 10%
sales.current <- sales.past[4] * (1 - 0.1);

# dividend per share in 2016, 2017, 2018, 2019
dividends.past <- c(0.67, 0.76, 0.70, 0.82);

# dividend per share in 2020 Q1, Q2, Q3, and Q4(E)
dividend.2020.q4 <- 0.08;
dividend.current <- 0.08 + 0.11 + 0.11 + dividend.2020.q4;

# net reserve for the start of 2021
reserves.current <- reserves.2020 - sales.current;


dividends <- c(dividends.past, dividend.current);

sales <- c(sales.past, sales.current);

# probability of sampling each historical data point
probs <- c(0.1, 0.1, 0.1, 0.5, 0.2);
probs <- probs / sum(probs);

# number of data points
n <- length(probs);


# Project one year of dividend and remaining reserves.
# @param reserves  initial reserves
project <- function(reserves) {
	# sample one data point from historical data of (dividends, sales)
	i <- sample.int(n, 1, prob=probs);

	if (sales[i] < reserves) {
		list(
			dividend = dividends[i],
			reserves = reserves - sales[i]
		)
	} else {
		list(
			# dividend must be discounted due to insufficient reserves
			dividend = dividends[i] * (reserves / sales[i]),
			# reserves are completely depleted
			reserves = 0
		)
	}
}

# Calculate present value of future cash flow
# @param cf  future cash flow
# @param r   discount rate
present_value <- function(cf, r) {
	n <- length(cf);
	cf / (1 + r)^(1:n)
}

# simulate a list of payouts
payouts.l <- lapply(
	1:B,
	function(b) {
		r <- reserves.current;
		payouts <- NULL;
		while (r > 0) {
			p <- project(r);
			payouts <- c(payouts, p$dividend);
			r <- p$reserves;
		}

		payouts
	}
);

# years remaining
years <- unlist(lapply(payouts.l, length));
table(years)

# total discounted cash flow
dcfs <- unlist(lapply(
	payouts.l,
	function(payouts) {
		sum(present_value(payouts * (1 - tax), discount)) +
			dividend.2020.q4 * (1 - tax)
	}
));

# total discounted cash flow under no tax scenario
dcfs.no.tax <- unlist(lapply(
	payouts.l,
	function(payouts) {
		sum(present_value(payouts, discount)) +
			dividend.2020.q4
	}
));


# estimate key statistics

profit.prob <- 0.95;
# entry price given target probability of profit
quantile(dcfs, 1 - profit.prob)
quantile(dcfs.no.tax, 1 - profit.prob)

price <- 3.03;
# expected margin of present value of profit given entry price
mean(dcfs / price) - 1
mean(dcfs.no.tax / price) - 1
# probability of profit given entry price
mean(dcfs > price)
mean(dcfs.no.tax > price)
# expected margin of present value of profit given profit > 0
mean(dcfs.no.tax[dcfs.no.tax > price]) / price - 1


# generate plots

hist(dcfs, breaks=20)
abline(v = price, col="red")

hist(dcfs.no.tax, breaks=20)
abline(v = price, col="red")

