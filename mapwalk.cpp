/*
 * mapwalk.cpp
 * (c) 2020 CincoNoveSeis Jornalismo Ltda.
 * 
 * This program is licensed under the GNU General Public License, version 3.
 * See the LICENSE file for details.
 */

#include <Rcpp.h>
#include <algorithm>
#include <random>

using namespace Rcpp;

DataFrame mapwalk_(NumericVector main,
                   DoubleVector bolsonaro_p,
                   List geom,
                   List g_geom,
                   std::set<int> all,
                   std::vector<int> useless)
{
    int orig_len = all.size();
    Rcout << "orig_len = " << orig_len << "\n";
    
    Function st_queen_sp("st_intersects_sp");
    Function st_sfc("st_sfc");
    
    List geom_q = List::create();
    for (int i = 0; i < geom.size(); i++) {
        if (!useless[i])
            geom_q.push_back(geom[i]);
    }
    geom_q = st_sfc(geom_q, Named("crs", 31983));
    
    List q_ = st_queen_sp(geom_q);
    
    List q = List::create();
    int j = 0;
    for (int i = 0; i < geom.size(); i++) {
        if (!useless[i]) {
            q.push_back(q_[j]);
            j += 1;
        }
        else
            q.push_back(IntegerVector());
    }
    
    std::random_device rd;
    std::mt19937 gen(rd());
    
    for (int i = 0; i < q.size(); i++) {
        if (useless[i])
            continue;
        
        NumericVector matches = q[i];
        std::vector<int> candidates;
        
        for (NumericVector::iterator j = matches.begin(); j != matches.end(); j++) {
            if (*j == i) continue;
            
            std::set<int>::iterator f = std::find(all.begin(), all.end(), *j);
            if (f == all.end())
                candidates.push_back(*j);
        }
        
        if (candidates.size() == 0) {
            useless[i] = true;
            continue;
        }
        
        std::uniform_int_distribution<int> dis(0, candidates.size() - 1);
        int pick = candidates[dis(gen)];
        all.insert(pick);
        
        main.push_back(main[i]);
        bolsonaro_p.push_back(NA_REAL);
        geom.push_back(g_geom[pick - 1]);
    }
    
    geom = st_sfc(geom, Named("crs", 31983));
    
    if (all.size() == orig_len) {
        Function st_sf("st_sf");
        return st_sf(Named("geom", geom),
                     Named("main", main),
                     Named("bolsonaro_p", bolsonaro_p));
    }
        
    return mapwalk_(main, bolsonaro_p, geom, g_geom, all, useless);
}

// [[Rcpp::export]]
DataFrame mapwalk(DataFrame spt, DataFrame sp, NumericVector all_vec) {
    std::vector<int> all_ = as<std::vector<int> >(all_vec);
    std::set<int> all(all_.begin(), all_.end());
    
    NumericVector main = spt["main"];
    DoubleVector bolsonaro_p = spt["freixo"];
    List geom = spt["geom"];
    List g_geom = sp["geom"];
    
    std::vector<int> useless(30000, false);
    
    return mapwalk_(main, bolsonaro_p, geom, g_geom, all, useless);
}