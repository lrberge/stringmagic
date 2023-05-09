#include "stringmagick.h"




std::vector<std::string> trim_ws(std::vector<std::string> x){

  int n = x.size();
  std::vector<std::string> res(x);

  std::string tmp, xi;
  for(int i=0 ; i<n ; ++i){
    xi = x[i];
    if(!xi.empty() && (is_blank(xi[0]) || is_blank(xi[xi.size() - 1]))){
      tmp = "";
      int n_xi = xi.size();

      while(n_xi >= 1 && is_blank(xi[n_xi - 1])) --n_xi;

      int  j = 0;
      while(j < n_xi && is_blank(xi[j])) ++j;

      while(j < n_xi) tmp += xi[j++];

      res[i] = tmp;
    }
  }

  return res;
}
