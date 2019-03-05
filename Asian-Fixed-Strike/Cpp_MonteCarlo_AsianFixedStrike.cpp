#include <Rcpp.h>
using namespace Rcpp;
//Using Monte Carlo simulation & Heston model to price Asian Fixed Strike call
//[[Rcpp::export]]
double priceCpp(double s0, double sigma, double r, double T, double K, double theta, double kappa, double xi, int trials){
    int m = 252*T;
    double ds , dnu, dt = T/m;
    NumericVector payoff(trials);
    NumericVector stockProcess(m + 1);
    NumericVector volProcess(m + 1);

    stockProcess[0] = s0;
    volProcess[0] = theta;

    for (int j = 0; j < trials; j++){
        for(int i = 0; i < m; i++){
            ds = r*stockProcess[i]*dt + sqrt(volProcess[i])*stockProcess[i]*sqrt(dt)*R::rnorm(0.0, 1.0);
            dnu = kappa*(theta - volProcess[i])*dt + xi*sqrt(volProcess[i])*sqrt(dt)*R::rnorm(0.0, 1.0);
            stockProcess[i + 1] = stockProcess[i] + ds;
            volProcess[i + 1] = std::max(volProcess[i] + dnu, 0.0);
        } 
        payoff[j] = std::max(mean(stockProcess) - K, 0.0);
    }
    return exp(-r*T)*mean(payoff);
}
