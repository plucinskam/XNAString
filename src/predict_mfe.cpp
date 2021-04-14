#include <Rcpp.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <ViennaRNA/fold.h>
#include <ViennaRNA/utils/basic.h>
#include <ViennaRNA/mfe.h>
#include <ViennaRNA/fold_compound.h>
#include <ViennaRNA/utils/strings.h>




using namespace Rcpp;

// [[Rcpp::export]]
StringVector RNAfold_MFE(StringVector seq) {
    /* The RNA sequence */
  
    std::string seq2 = Rcpp::as<std::string>(seq); 
    char const *sequence = seq2.c_str();
    
    char  *structure = (char*) malloc((strlen(sequence) + 1) * sizeof(char));
    
    /* predict Minimum Free Energy and corresponding secondary structure */
    float mfe = vrna_fold(sequence, structure);
    
    std::string mfe_str = std::to_string(mfe);
    std::string str(structure);
    
    Rcpp::StringVector results_vector(2);
    
    results_vector[0] = structure;
    results_vector[1] = mfe_str ;
    
    return results_vector;
}




// [[Rcpp::export]]
StringVector RNAcofold_MFE(StringVector seq) {
  /* The RNA sequence */

  std::string seq2 = Rcpp::as<std::string>(seq);
  char const *sequence = seq2.c_str();

  char  *structure = (char*) malloc((strlen(sequence) + 1) * sizeof(char));

  /* predict Minimum Free Energy and corresponding secondary structure */
  float mfe = vrna_cofold(sequence, structure);

  std::string mfe_str = std::to_string(mfe);
  std::string str(structure);

  Rcpp::StringVector results_vector(2);

  results_vector[0] = structure;
  results_vector[1] = mfe_str ;

  return results_vector;
}





