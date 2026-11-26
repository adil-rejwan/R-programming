##Question: A lot consisting of 2200 items, a sample of size 225 is taken. If it contains 14 or less defectives,
#The lot is accepted otherwise rejected. Plot the OC, ATI and AOQ curves. Also obtain the value of A.O.Q.L.
N <- 2200
n <- 225
c <- 14

p <- seq(0, 0.15, by = 0.005)
lambda <- n * p

Pa  <- ppois(c, lambda)
AOQ <- p * Pa * ((N - n) / N)
ATI <- n * Pa + N * (1 - Pa)

AOQL   <- max(AOQ)
p_AOQL <- p[which.max(AOQ)]
AOQL
p_AOQL
plot(p*100, Pa, type = "l", lwd = 2,
     xlab = "Incoming Fraction Defective (%)",
     ylab = "Probability of Acceptance", main = "OC Curve")
plot(p*100, AOQ*100, type = "l", lwd = 2,
     xlab = "Incoming Fraction Defective (%)",
     ylab = "AOQ (%)", main = "AOQ Curve")
abline(h = AOQL*100, lty = 2)
plot(p*100, ATI, type = "l", lwd = 2,
     xlab = "Incoming Fraction Defective (%)",
     ylab = "Average Total Inspection", main = "ATI Curve")

##Question: A student wants to prepare a daily diet using two food items, Rice and Eggs. 
#The student wants to satisfy the minimum daily requirements of protein and calories at the minimum possible cost.
#Food Item	Quantity (per unit)	Protein (g)	Calories (kcal)	Cost (Tk.)
#Rice	100 g	3	200	20
#Egg	100 g	13	155	25
#Minimum Daily Requirement	—	30	1,000	—
#Let X1 = number of 100 g units of Rice per day, X2 = number of 100 g units of Egg per day.
#Minimize Z = 20X1 + 25X2
#Subject to: 3X1 + 13X2 ≥ 30; 200X1 + 155X2 ≥ 1,000; X1, X2 ≥ 0
#Solve the above LPP by using the Graphical Method with the help of R and determine the minimum daily cost of the diet.

library(lpSolve)
library(ggplot2)

objective   <- c(20, 25)
constraints <- matrix(c(3, 13,
                        200, 155), nrow = 2, byrow = TRUE)
rhs       <- c(30, 1000)
direction <- c(">=", ">=")

solution <- lp("min", objective, constraints, direction, rhs, compute.sens = TRUE)
solution
optimal_values <- solution$solution
min_cost       <- solution$objval
optimal_values
min_cost

constraint1 <- function(x) (30 - 3 * x) / 13
constraint2 <- function(x) (1000 - 200 * x) / 155

x_values <- seq(0, 12, length.out = 400)
df <- data.frame(x  = x_values,
                 y1 = constraint1(x_values),
                 y2 = constraint2(x_values))
opt_df <- data.frame(x = optimal_values[1], y = optimal_values[2])

ggplot(df, aes(x = x)) +
  geom_line(aes(y = y1), color = "blue", linewidth = 1.2) +
  geom_line(aes(y = y2), color = "red",  linewidth = 1.2) +
  geom_ribbon(aes(ymin = pmax(0, y1, y2), ymax = Inf),
              fill = "lightgreen", alpha = 0.4) +
  geom_point(data = opt_df, aes(x = x, y = y), color = "black", size = 4) +
  labs(title = "Feasible Region and Optimal Solution (Minimization)",
       x = "X1 (Rice, 100 g units)", y = "X2 (Egg, 100 g units)") +
  xlim(0, 12) + 
  ylim(0, 12) +
  theme_minimal()


#Question: Construct appropriate control chart-

#Lot Number	Number Inspected	Number of Defective
1	        500                  	25
2		      400                  	42
3	       	300                  35
4		       150	               	16
5		       600                	15
6		       450                	40
7		       750	              	72
8	         	800	              	81
9	        	900	               	82
10	      	1000                	100

#Estimate the process average fraction defective and plot it.
n_i       <- c(500, 400, 300, 150, 600, 450, 750, 800, 900, 1000)
defective <- c(25, 42, 35, 16, 15, 40, 72, 81, 82, 100)
lots <- 1:length(defective)

p_i   <- defective / n_i
p_bar <- sum(defective) / sum(n_i)
p_bar

SE  <- sqrt(p_bar * (1 - p_bar) / n_i)
UCL <- p_bar + 3 * SE
LCL <- pmax(0, p_bar - 3 * SE)

plot(lots, p_i, type = "b", pch = 19, col = "blue",
     ylim = c(0, max(p_i, UCL) * 1.2),
     main = "p-Chart (Variable Sample Size)",
     xlab = "Lot Number", ylab = "Fraction Defective")
abline(h = p_bar, col = "darkgreen", lwd = 2)
lines(lots, UCL, type = "s", col = "red", lty = 2, lwd = 2)
lines(lots, LCL, type = "s", col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("p_i (each lot)", "Center Line", "UCL/LCL"),
       col = c("blue", "darkgreen", "red"), lty = c(1, 1, 2),
       pch = c(19, NA, NA), bty = "n")

which(p_i > UCL | p_i < LCL)

##Question: Construct a control chart for mean and range for the following data on the basis of fuses, 
#sample of 4 being taken every hour. Solve and visualize it by using R.

#Sample	Observations
1     	27, 23, 36, 24
2	     32, 24, 30, 27
3	     22, 22, 28, 24
4     	35, 30, 31, 40
5	     50, 48, 42, 32
6	     34, 22, 20, 18
7     	28, 28, 32, 38
8     	45, 42, 47, 12
9	     36, 41, 44, 12
10	     30, 24, 22, 19
11	     32, 28, 27, 25
12	     44, 43, 46, 42
13	     38, 42, 38, 39
14     	30, 28, 38, 26
15	     25, 22, 18, 24
16	     40, 39, 48, 45
17	     38, 44, 43, 35

data <- matrix(c(
  27,23,36,24,  32,24,30,27,  22,22,28,24,  35,30,31,40,
  50,48,42,32,  34,22,20,18,  28,28,32,38,  45,42,47,12,
  36,41,44,12,  30,24,22,19,  32,28,27,25,  44,43,46,42,
  38,42,38,39,  30,28,38,26,  25,22,18,24,  40,39,48,45,
  38,44,43,35), byrow = TRUE, ncol = 4)

sample.means  <- rowMeans(data)
sample.ranges <- apply(data, 1, function(x) max(x) - min(x))
Xbarbar <- mean(sample.means)
Rbar    <- mean(sample.ranges)

A2 <- 0.729; D3 <- 0; D4 <- 2.282

CL_x  <- Xbarbar
UCL_x <- Xbarbar + A2 * Rbar
LCL_x <- Xbarbar - A2 * Rbar

CL_r  <- Rbar
UCL_r <- D4 * Rbar
LCL_r <- D3 * Rbar
plot(sample.means, type = "b", pch = 19, col = "purple",
     main = "X_bar Chart", xlab = "Sample No.", ylab = "Sample mean")
abline(h = CL_x, col = "darkgreen", lty = 2)
abline(h = UCL_x, col = "red", lty = 2)
abline(h = LCL_x, col = "red", lty = 2)
legend("bottomright", legend = c("Sample Mean", "Center Line", "UCL/LCL"),
       col = c("purple", "darkgreen", "red"), lty = c(1, 1, 2),
       pch = c(19, NA, NA), bty = "n")

plot(sample.ranges, type = "b", pch = 19, col = "navy",
     main = "R Chart", xlab = "Sample No.", ylab = "Sample range")
abline(h = CL_r, col = "darkgreen", lty = 2)
abline(h = UCL_r, col = "red", lty = 2)
abline(h = LCL_r, col = "red", lty = 2)

which(sample.means > UCL_x | sample.means < LCL_x)
which(sample.ranges > UCL_r | sample.ranges < LCL_r)


data=matrix(c(
  27,23,36,14,
  28, 30, 30, 17,
  21, 22, 41, 44,
  42, 35, 28, 40,
  51, 23, 21, 34,
  33, 34, 39, 30,
  30, 22, 18, 12,
  35, 48, 20, 47,
  20, 34, 15, 42,
  22, 50, 45, 41,
  34, 32, 48, 22,
  44, 24, 32, 33,
  30, 28, 28, 25,
  30, 38, 38, 52,
  36, 32, 39, 45,
  44, 48, 33, 34,
  39, 44, 29, 40,
  38, 44, 32, 27,
  39, 34, 32, 28,
  38, 34, 36, 30),
  byrow = T,ncol=4)
data
sample.means=rowMeans(data)
sample.ranges=apply(data,1,function(x) max(x)-min(x))
Xbarbar=mean(sample.means)
Rbar=mean(sample.ranges)
A2=0.729
D3=0
D4=2.282

CL=Xbarbar
UCL=Xbarbar+A2*Rbar
LCL=Xbarbar-A2*Rbar

plot(sample.means,type="b",pch=19,col="purple", main="X_bar Chart", xlab="sample No.", ylab = "sample mean")
abline(h=CL, type="b",col="darkgreen",lty=2)
abline(h=UCL, type="b",col="red",lty=2)
abline(h=LCL, type="b",col="red",lty=2)
legend("bottomright",legend=c("Sample Range","Center Line","UCL/LCL"),col=c("purple","darkgreen","red"),lty=c(1,1,2), pch=c(19,NA,NA), bty="n")






p-chart for Spark Plug Inspection
# ================================

# ---- Step 1: Enter the data (number of defectives per lot) ----
defectives <- c(5,3,15,10,3,16,4,10,5,8,
                5,4,6,8,7,4,6,8,6,10)

n <- 100   # sample size per lot
lots <- 1:length(defectives)

# ---- Step 2: Compute proportion defective per lot ----
p_i <- defectives / n

# ---- Step 3: Compute overall average proportion defective ----
p_bar <- sum(defectives) / (length(defectives)*n)

# ---- Step 4: Compute control limits ----
SE <- sqrt(p_bar * (1 - p_bar) / n)
UCL <- p_bar + 3*SE
LCL <- p_bar - 3*SE
if (LCL < 0) LCL <- 0

# ---- Step 5: Plot the p-chart ----
plot(lots, p_i, type="b", pch=19, col="blue",
     ylim=c(0, max(p_i,UCL)*1.3),
     main="p-Chart for Spark Plug Inspection",
     xlab="Lot Number", ylab="Fraction Defective")

# Add center line and control limits
abline(h=p_bar, col="darkgreen", lwd=2)
abline(h=UCL, col="red", lty=2, lwd=2)
abline(h=LCL, col="red", lty=2, lwd=2)

# Add legend
legend("topright", legend=c("p_i (each lot)","Center Line","UCL/LCL"),
       col=c("blue","darkgreen","red"),
       lty=c(1,1,2), pch=c(19,NA,NA), bty="n")



N <- 10000
n <- 200
c <- 2

p <- seq(0, 0.05, by = 0.001)

lambda <- n * p

Pa <- ppois(c, lambda)

AOQ <- p * Pa * ((N - n) / N)
AOQL <- max(AOQ)
p_AOQL <- p[which.max(AOQ)]

ATI <- n * Pa + N * (1 - Pa)

plot(p * 100, Pa,
     type = "l",
     lwd = 2,
     xlab = "Incoming Fraction Defective (%)",
     ylab = "Probability of Acceptance",
     main = "OC Curve - Poisson Sampling")

plot(p * 100, AOQ * 100,
     type = "l",
     lwd = 2,
     xlab = "Incoming Fraction Defective (%)",
     ylab = "AOQ (%)",
     main = "AOQ Curve - Poisson Sampling")

abline(h = AOQL * 100, lty = 2)

plot(p * 100, ATI,
     type = "l",
     lwd = 2,
     xlab = "Incoming Fraction Defective (%)",
     ylab = "Average Total Inspection",
     main = "ATI Curve - Poisson Sampling")







#For maximization objective function
library(lpSolve)
objective <- c(50, 40)
constraints <- matrix(c(2, 3, 3, 2), nrow = 2, byrow = TRUE)
rhs <- c(100, 90)
direction <- c("<=", "<=")

solution <- lp("max", objective, constraints, direction, rhs, compute.sens = TRUE)
solution
optimal_values <- solution$solution
max_profit <- solution$objval


library(ggplot2)
constraint1 <- function(x) (100 - 2 * x) / 3
constraint2 <- function(x) (90 - 3 * x) / 2

# Define the range of x values
x_values <- seq(0, 50, length.out = 400)
# Create a data frame for ggplot
df <- data.frame(x = x_values,
                 y1 = pmax(0, constraint1(x_values)),
                 y2 = pmax(0, constraint2(x_values)))

# Plot the constraints and feasible region
ggplot(df, aes(x = x)) +
  geom_line(aes(y = y1), color = "blue", size = 1) +
  geom_line(aes(y = y2), color = "red", size = 1) +
  geom_ribbon(aes(ymin = 0, ymax = pmin(y1, y2)), fill = "pink", alpha = 0.5) +
  geom_point(aes(x = optimal_values[1], y = optimal_values[2]), color = "green", size = 3) +
  labs(title = "Feasible Region and Optimal Solution",
       x = "Units of P1",
       y = "Units of P2") +
  annotate("text", x = optimal_values[1], y = optimal_values[2], label = "Optimal Solution", hjust = -0.2, vjust = -1.5) +
  xlim(0, 50) +
  ylim(0, 50) +
  theme_minimal()



########
########

#For minimization objective function
########
########
library(lpSolve)

# Objective function coefficients
objective <- c(6, 4)

# Constraint matrix
constraints <- matrix(c(2, 1,
                        1, 2),
                      nrow = 2, byrow = TRUE)

# Right-hand side
rhs <- c(8, 7)
direction <- c(">=", ">=")
solution <- lp("min", objective, constraints, direction, rhs, compute.sens = TRUE)

solution
optimal_values <- solution$solution
min_cost <- solution$objval

library(ggplot2)

# Constraint boundary functions
constraint1 <- function(x) (8 - 2*x)
constraint2 <- function(x) (7 - 1*x) / 2

# Generate x values
x_values <- seq(0, 10, length.out = 400)

# Data frame for plotting
df <- data.frame(
  x = x_values,
  y1 = constraint1(x_values),
  y2 = constraint2(x_values)
)

# Keep only non-negative values

# Create separate data frame for optimal solution
opt_df <- data.frame(
  x = optimal_values[1],
  y = optimal_values[2]
)

ggplot(df, aes(x = x)) +
  geom_line(aes(y = y1), color = "blue", size = 1.2) +
  geom_line(aes(y = y2), color = "red", size = 1.2) +
  geom_ribbon(aes(ymin =pmax(y1, y2) , ymax = Inf),
              fill = "lightgreen", alpha = 0.4) +
  geom_point(data = opt_df, aes(x = x, y = y),
             color = "black", size = 4) +
  labs(
    title = "Feasible Region and Optimal Solution (Minimization)",
    x = "Units of Material 1",
    y = "Units of Material 2"
  ) +
  xlim(0, 10) +
  ylim(0, 10) +
  theme_minimal()


###############################################################
# PART A: ACCEPTANCE SAMPLING (Single Sampling Plan, Poisson)
###############################################################
# QUESTION A
# A lot of N = 10000 items is inspected using a single sampling plan
# with sample size n = 200 and acceptance number c = 2.
# Rejected lots are 100% inspected and defectives are replaced by good items.
# Use the Poisson approximation. For incoming fraction defective p = 0 to 0.05
# (step 0.001):
#  (i)   Compute and plot the OC curve (Pa vs p).
#  (ii)  Compute and plot the AOQ curve, and find the AOQL and the p at which it occurs.
#  (iii) Compute and plot the ATI curve.
#  (iv)  Find Pa, AOQ and ATI at p = 0.005, 0.01, 0.02 and 0.03.
###############################################################

N <- 10000                      # Lot size (total items in one lot)
n <- 200                        # Sample size drawn from each lot
c <- 2                          # Acceptance number (accept lot if defectives <= 2)

p <- seq(0, 0.05, by = 0.001)   # Grid of incoming fraction defective: 0, 0.001, ..., 0.05

lambda <- n * p                 # Poisson mean = n*p (expected defectives in the sample)

Pa <- ppois(c, lambda)          # Pa = P(X <= c) = probability of accepting the lot (OC curve)

AOQ <- p * Pa * ((N - n) / N)   # Average Outgoing Quality: p * Pa * (N-n)/N
AOQL <- max(AOQ)                # AOQL = worst (maximum) value of AOQ over all p
p_AOQL <- p[which.max(AOQ)]     # The p at which AOQ is maximum

ATI <- n * Pa + N * (1 - Pa)    # Average Total Inspection: n if accepted, N if rejected

# ---------- Plot 1: OC curve ----------
plot(p * 100, Pa,                                   # x = p in percent, y = Pa
     type = "l",                                    # draw as a line
     lwd = 2,                                       # line thickness
     xlab = "Incoming Fraction Defective (%)",      # x-axis label
     ylab = "Probability of Acceptance",            # y-axis label
     main = "OC Curve - Poisson Sampling")          # plot title

# ---------- Plot 2: AOQ curve ----------
plot(p * 100, AOQ * 100,                            # x = p in percent, y = AOQ in percent
     type = "l",                                    # draw as a line
     lwd = 2,                                       # line thickness
     xlab = "Incoming Fraction Defective (%)",      # x-axis label
     ylab = "AOQ (%)",                              # y-axis label
     main = "AOQ Curve - Poisson Sampling")         # plot title

abline(h = AOQL * 100, lty = 2)                     # dashed horizontal line at AOQL

# ---------- Plot 3: ATI curve ----------
plot(p * 100, ATI,                                  # x = p in percent, y = ATI
     type = "l",                                    # draw as a line
     lwd = 2,                                       # line thickness
     xlab = "Incoming Fraction Defective (%)",      # x-axis label
     ylab = "Average Total Inspection",             # y-axis label
     main = "ATI Curve - Poisson Sampling")         # plot title

# ---------- Part (iv): values at selected p ----------
p_sel <- c(0.005, 0.01, 0.02, 0.03)                 # the four p values asked in the question
Pa_sel <- ppois(c, n * p_sel)                       # Pa at these p values
AOQ_sel <- p_sel * Pa_sel * ((N - n) / N)           # AOQ at these p values
ATI_sel <- n * Pa_sel + N * (1 - Pa_sel)            # ATI at these p values
data.frame(p = p_sel, Pa = Pa_sel, AOQ = AOQ_sel, ATI = ATI_sel)  # show as a table

AOQL                                                # print AOQL
p_AOQL                                              # print p where AOQL occurs

# ANSWER A
#  AOQL   = 0.006713 (about 0.67%), occurring at p = 0.011 (1.1%)
#           (Pa at that p = 0.6227)
#
#   p      Pa       AOQ       ATI
#  0.005  0.9197   0.004507   986.95
#  0.010  0.6767   0.006631   3368.57
#  0.020  0.2381   0.004667   7666.59
#  0.030  0.0620   0.001822   9392.71
#
#  Interpretation: as p rises, Pa falls, so more lots are 100% inspected
#  and ATI climbs toward N. AOQ rises, peaks at the AOQL, then falls
#  because bad lots get screened and defectives replaced.


###############################################################
# PART B: LPP - MAXIMIZATION (Graphical + lpSolve)
###############################################################
# QUESTION B
# A factory makes two products P1 and P2. Profit is Tk 50 per unit of P1
# and Tk 40 per unit of P2. Resource 1 usage: 2 (P1) and 3 (P2), available 100.
# Resource 2 usage: 3 (P1) and 2 (P2), available 90.
#  (i)   Formulate the LPP.
#  (ii)  Find the optimal production plan and maximum profit.
#  (iii) Draw the feasible region and mark the optimal point.
#  (iv)  Report the shadow prices (dual values) of the two resources.
#
# Formulation:
#   Max Z = 50 x1 + 40 x2
#   s.t.  2 x1 + 3 x2 <= 100
#         3 x1 + 2 x2 <= 90
#         x1, x2 >= 0
###############################################################
install.packages("lpSolve")   # package name must be in quotes
library(lpSolve)              # load the LP solver package                                   # load the LP solver package

objective <- c(50, 40)                              # profit coefficients of x1 and x2
constraints <- matrix(c(2, 3, 3, 2), nrow = 2, byrow = TRUE)  # rows = constraints, cols = x1, x2
rhs <- c(100, 90)                                   # right-hand sides (resource availability)
direction <- c("<=", "<=")                          # both constraints are of <= type

solution <- lp("max", objective, constraints, direction, rhs, compute.sens = TRUE)  # solve, with sensitivity
solution                                            # prints the optimal objective value
optimal_values <- solution$solution                 # optimal x1 and x2
max_profit <- solution$objval                       # maximum profit
optimal_values                                      # print x1, x2
max_profit                                          # print Z
solution$duals                                      # shadow prices (first 2) and reduced costs (last 2)

library(ggplot2)                                    # load ggplot2 for plotting

constraint1 <- function(x) (100 - 2 * x) / 3        # x2 from constraint 1: x2 = (100 - 2 x1)/3
constraint2 <- function(x) (90 - 3 * x) / 2         # x2 from constraint 2: x2 = (90 - 3 x1)/2

x_values <- seq(0, 50, length.out = 400)            # 400 x1 values between 0 and 50

df <- data.frame(x = x_values,                      # data frame holding x1 values
                 y1 = pmax(0, constraint1(x_values)),  # constraint 1 line (negatives cut to 0)
                 y2 = pmax(0, constraint2(x_values)))  # constraint 2 line (negatives cut to 0)

ggplot(df, aes(x = x)) +                            # start plot with x1 on x-axis
  geom_line(aes(y = y1), color = "blue", size = 1) +   # draw constraint 1 in blue
  geom_line(aes(y = y2), color = "red", size = 1) +    # draw constraint 2 in red
  geom_ribbon(aes(ymin = 0, ymax = pmin(y1, y2)),      # shade region under both lines
              fill = "pink", alpha = 0.5) +            # feasible region colour and transparency
  geom_point(aes(x = optimal_values[1], y = optimal_values[2]),  # mark the optimal point
             color = "green", size = 3) +
  labs(title = "Feasible Region and Optimal Solution", # plot title
       x = "Units of P1",                              # x-axis label
       y = "Units of P2") +                            # y-axis label
  annotate("text", x = optimal_values[1], y = optimal_values[2],  # text label near the point
           label = "Optimal Solution", hjust = -0.2, vjust = -1.5) +
  xlim(0, 50) +                                        # limit x-axis to 0-50
  ylim(0, 50) +                                        # limit y-axis to 0-50
  theme_minimal()                                      # clean plot theme

# ANSWER B
#  Corner point where both constraints bind:
#    2x1 + 3x2 = 100  and  3x1 + 2x2 = 90
#    Solving gives x1 = 14, x2 = 24
#  Max profit Z = 50(14) + 40(24) = 700 + 960 = Tk 1660
#  Shadow prices: Resource 1 = 4, Resource 2 = 14
#    (one extra unit of Resource 1 raises profit by Tk 4, of Resource 2 by Tk 14)


###############################################################
# PART C: LPP - MINIMIZATION (Graphical + lpSolve)
###############################################################
# QUESTION C
# A feed mix uses Material 1 (cost 6 per unit) and Material 2 (cost 4 per unit).
# Nutrient A: 2 per unit of M1 and 1 per unit of M2, at least 8 required.
# Nutrient B: 1 per unit of M1 and 2 per unit of M2, at least 7 required.
#  (i)   Formulate the LPP.
#  (ii)  Find the minimum-cost mix and the minimum cost.
#  (iii) Draw the feasible region and mark the optimal point.
#  (iv)  Report the shadow prices of the two nutrient requirements.
#
# Formulation:
#   Min Z = 6 x1 + 4 x2
#   s.t.  2 x1 + x2  >= 8
#         x1 + 2 x2  >= 7
#         x1, x2 >= 0
###############################################################

library(lpSolve)                                    # load LP solver (safe to reload)

objective <- c(6, 4)                                # cost coefficients of x1 and x2

constraints <- matrix(c(2, 1,                       # constraint 1 coefficients: 2 x1 + 1 x2
                        1, 2),                      # constraint 2 coefficients: 1 x1 + 2 x2
                      nrow = 2, byrow = TRUE)       # fill the matrix row by row

rhs <- c(8, 7)                                      # minimum requirements
direction <- c(">=", ">=")                          # both constraints are of >= type

solution <- lp("min", objective, constraints, direction, rhs, compute.sens = TRUE)  # solve the minimization
solution                                            # prints the optimal (minimum) cost
optimal_values <- solution$solution                 # optimal x1 and x2
min_cost <- solution$objval                         # minimum cost
optimal_values                                      # print x1, x2
min_cost                                            # print Z
solution$duals                                      # shadow prices and reduced costs

library(ggplot2)                                    # load ggplot2 (safe to reload)

constraint1 <- function(x) (8 - 2 * x)              # x2 from constraint 1: x2 = 8 - 2 x1
constraint2 <- function(x) (7 - 1 * x) / 2          # x2 from constraint 2: x2 = (7 - x1)/2

x_values <- seq(0, 10, length.out = 400)            # 400 x1 values between 0 and 10

df <- data.frame(                                   # data frame for the two boundary lines
  x = x_values,                                     # x1 values
  y1 = constraint1(x_values),                       # constraint 1 line
  y2 = constraint2(x_values)                        # constraint 2 line
)

opt_df <- data.frame(                               # separate data frame for the optimal point
  x = optimal_values[1],                            # optimal x1
  y = optimal_values[2]                             # optimal x2
)

ggplot(df, aes(x = x)) +                            # start plot with x1 on x-axis
  geom_line(aes(y = y1), color = "blue", size = 1.2) +  # constraint 1 in blue
  geom_line(aes(y = y2), color = "red", size = 1.2) +   # constraint 2 in red
  geom_ribbon(aes(ymin = pmax(y1, y2), ymax = Inf),     # shade region ABOVE both lines (>= type)
              fill = "lightgreen", alpha = 0.4) +
  geom_point(data = opt_df, aes(x = x, y = y),          # mark optimal point in black
             color = "black", size = 4) +
  labs(
    title = "Feasible Region and Optimal Solution (Minimization)",  # plot title
    x = "Units of Material 1",                          # x-axis label
    y = "Units of Material 2"                           # y-axis label
  ) +
  xlim(0, 10) +                                         # limit x-axis to 0-10
  ylim(0, 10) +                                         # limit y-axis to 0-10
  theme_minimal()                                       # clean plot theme

# ANSWER C
#  Corner point where both constraints bind:
#    2x1 + x2 = 8  and  x1 + 2x2 = 7
#    Solving gives x1 = 3, x2 = 2
#  Min cost Z = 6(3) + 4(2) = 18 + 8 = 26
#  Shadow prices: Nutrient A = 8/3 (about 2.667), Nutrient B = 2/3 (about 0.667)
#    (one extra unit of requirement raises the minimum cost by that amount)



















