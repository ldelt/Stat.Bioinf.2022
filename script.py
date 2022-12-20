# В этом коде мы считаем, что в последний столбец все же записан клеточный тип

import pandas as pd
import numpy as np
import scipy.stats as st
from statsmodels.stats.weightstats import ztest


first_cell_type_expressions_path = input('путь до таблицы с экспрессиями генов для первого клеточного типа:')
second_cell_type_expressions_path = input('путь до таблицы с экспрессиями генов для второго клеточного типа:')
save_results_table = input('Имя таблицы с результатами:')

first_cell_type_expressions = pd.read_csv(first_cell_type_expressions_path)
second_cell_type_expressions = pd.read_csv(second_cell_type_expressions_path)


def check_dge_with_ci(first_table, second_table):
    
    ci_test_results = []
    
    for col in list(first_table.columns)[0:-1]:
    
        ci_b = st.t.interval(alpha=0.95, 
                  df=len(first_table[col]) - 1, 
                  loc=np.mean(first_table[col]), 
                  scale=st.sem(first_table[col])) 
    
        ci_nk = st.t.interval(alpha=0.95, 
                  df=len(second_table[col]) - 1, 
                  loc=np.mean(second_table[col]), 
                  scale=st.sem(second_table[col])) 
    
        ci_test_result = check_intervals_intersect(ci_b, ci_nk)
        ci_test_results.append(ci_test_result)

    return ci_test_results


def check_dge_with_ztest(first_table, second_table):
    
    z_test_results = []
    z_test_p_values = []
    
    for col in list(first_table.columns)[0:-1]:
        
        dge = ztest(first_table[col], second_table[col])
        z_test_p_values.append(dge[1])
        
        if dge[1] < 0.05:
            z_test_results.append(False)
        else: 
            z_test_results.append(True)
            
    return (z_test_results, z_test_p_values)


def mean_d(first_table, second_table):
    
    mean_diff = []
    
    for col in list(first_table.columns)[0:-1]:
        
        md = np.mean(second_table[col]) - np.mean(first_table[col])
        mean_diff.append(md)
    
    return(mean_diff)


ci_test_results = check_dge_with_ci(first_cell_type_expressions, second_cell_type_expressions)
z_test_results, z_test_p_values = check_dge_with_ztest(first_cell_type_expressions, second_cell_type_expressions)
mean_diff = mean_d(first_cell_type_expressions, second_cell_type_expressions)

results = {
    "ci_test_results": ci_test_results,
    "z_test_results": z_test_results,
    "z_test_p_values": z_test_p_values,
    "mean_diff": mean_diff
}

results = pd.DataFrame(results)

results.to_csv(f"{save_results_table}.csv")