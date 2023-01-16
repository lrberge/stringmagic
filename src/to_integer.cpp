#include <stdint.h>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// <stdint.h>: uint32_t

//
// Very cool hashing algorithm that I found thanks to Sebastian Krantz's `collapse`.
// The author of the hashing idea is Morgan Jacob methinks.
//

// unions: only one space in memory. Interprets the data in memory in different ways.
union d2int {
  double dbl;
  uint32_t uint[2];
};

inline uint32_t hash_to_int(uint32_t value, int shifter){
  return (3141592653U * value >> (32 - (shifter)));
}

inline uint32_t get_value_sexp_i(int type_x, int *px_int, double *px_dbl, intptr_t *px_intptr,
                                 int i, union d2int u_d2int){
  // we extract the value from a RSEXP

  uint32_t value = 0;
  if(type_x == REALSXP){
    u_d2int.dbl = px_dbl[i];
    value = u_d2int.uint[0] + u_d2int.uint[1];
  } else if(type_x == INTSXP){
    value = px_int[i];
  } else if(type_x == STRSXP){
    value = px_intptr[i] & 0xffffffff;
  } else {
    Rf_error("wrong type: internal error");
  }

  return value;
}

inline bool is_same_xi(int type_x, int *px_int, double *px_dbl, intptr_t *px_intptr,
                       int i, int j){
  // are two values of x identical?
  // this function is type agnostic

  bool res = false;
  if(type_x == REALSXP){
    res = px_dbl[i] == px_dbl[j];
  } else if(type_x == INTSXP){
    res = px_int[i] == px_int[j];
  } else if(type_x == STRSXP){
    res = px_intptr[i] == px_intptr[j];
  } else {
    Rf_error("wrong type: internal error");
  }

  return res;
}

// [[Rcpp::export]]
IntegerVector cpp_to_integer(SEXP x){
  // only accepts atomic vectors, of type: int/real/char

  int n = Rf_length(x);
  int type_x = TYPEOF(x);

  // we find out the number of bits (see shifter)
  // we find the first multiple of 2 greater than n
  const size_t n2 = 2U * (size_t) n;
  size_t n2_powOf2 = 256;
  int shifter = 8;

  while (n2_powOf2 < n2) {
    n2_powOf2 *= 2;
    ++shifter;
  }

  std::vector<int> h(n2_powOf2, -1);
  IntegerVector res(n, 0);
  uint32_t id = 0;
  union d2int u_d2int;
  int g = 0;

  void * px;
  if(type_x == INTSXP){
    px = INTEGER(x);
  } else if(type_x == REALSXP){
    px = REAL(x);
  } else if(type_x == STRSXP){
    px = STRING_PTR(x);
  } else {
    Rf_error("Internal error: wrong type in x.");
  }

  int *px_int = (int *) px;
  double *px_dbl = (double *) px;
  intptr_t *px_intptr = (intptr_t *) px;

  uint32_t value = 0;
  for(int i=0 ; i<n ; ++i){
    value = get_value_sexp_i(type_x, px_int, px_dbl, px_intptr, i, u_d2int);
    id = hash_to_int(value, shifter);

    bool does_exist = false;
    // here this is done to avoid collision
    // when there is a collision, we move forward
    // -1 is when h is not yet set
    while(h[id] != - 1){
      // does the value exist already?
      if(is_same_xi(type_x, px_int, px_dbl, px_intptr, h[id], i)){
        res[i] = res[h[id]];
        does_exist = true;
        break;

      } else {
        // we increment the id (the hash already exists for a different value!)
        ++id;
        if(id > n2_powOf2){
          id %= n2_powOf2;
        }
      }
    }

    if(!does_exist){
      // hash never seen => ok
      h[id] = i;
      res[i] = ++g;
    }
  }

  return res;
}













// [[Rcpp::export]]
IntegerVector cpp_combine_clusters(SEXP cluster_list, IntegerVector index){
  // cluster: list of integer vectors, each ranging from 1 to the number of cases
  // index: result of order() on the clusters

  if(TYPEOF(cluster_list) != VECSXP){
    stop("Internal error: Only lists are accepted!");
  }

  int Q = Rf_length(cluster_list);

  int n = index.size();
  IntegerVector res(n);

  // Loading the data
  std::vector<int*> pcluster(Q);
  for(int q=0 ; q<Q ; ++q){
    SEXP cluster_q = VECTOR_ELT(cluster_list, q);

    pcluster[q] = INTEGER(cluster_q);
  }

  // the observation ID
  int obs = index[0] - 1;

  // vector holding the current value
  std::vector<int> current_value(Q);

  // initialization
  int counter = 1;
  res[obs] = counter;
  for(int q=0 ; q<Q ; ++q){
    current_value[q] = pcluster[q][obs];
  }

  // we loop on the vector and flag values that are different
  int q = 0;
  for(int i=1 ; i<n ; ++i){
    obs = index[i] - 1;

    for(q=0 ; q<Q ; ++q){
      if(pcluster[q][obs] != current_value[q]){
        break;
      }
    }

    // if the condition holds => means the values are different
    if(q < Q){
      ++counter;
      // we save the new values
      for(; q<Q ; ++q){
        current_value[q] = pcluster[q][obs];
      }
    }

    res[obs] = counter;
  }

  return res;
}

// [[Rcpp::export]]
IntegerVector cpp_create_pos(IntegerVector index){
  // index: index vector of consecutive values
  // output: 1 2 3 1 1 2 for input 1 1 1 2 3 4 4

  int n = index.size();
  IntegerVector res(n, 1);
  int current = index[0];
  int val = 1;

  for(int i=1 ; i<n ; ++i){
    if(index[i] == current){
      ++val;
    } else {
      current = index[i];
      val = 1;
    }

    res[i] = val;
  }

  return res;
}


























