# Course Transaction Data Procrastination Analysis

Supplementary repository of the short paper "Are You an Early Dropper or Late Shopper? Mining Enrollment Transaction Data to Study Procrastination in Higher Education" accepted at EDM '24.

Citation:

Borchers, C., Xu, Y., & Pardos, Z. A. (2024). Are You an Early Dropper or Late Shopper? Mining Enrollment Transaction Data to Study Procrastination in Higher Education. In *Proceedings of the 17th International Conference on Educational Data Mining (EDM)*. Atlanta, GA, USA.
```
@inproceedings{borchers2024early,
  title={Are You an Early Dropper or Late Shopper? Mining Enrollment Transaction Data to Study Procrastination in Higher Education},
  author={Borchers, Conrad and Xu, Yinuo and Pardos, Zachary A},
  booktitle={Proceedings of the 17th International Conference on Educational Data Mining},
  year={2024}
}
```

## Folder Structure

* `enroll_activities_trends.ipynb`: calculates and graphs weekly enrollment activities (adds, drops, waitlists, swaps) trends
* `student_basket_workload_calculations.ipynb`: calculates weekly/daily student basket size (number of courses students are enrolled in at any given point in time), and the predicted course load and credit hours of their basket.
* `proccrastination_analysis.ipynb`: calculates procrastination and regularity indices and identifies procrastinator student-semester pairs.
* `export-for-R.ipynb`: aggregates variables on the student-semester level into a table for regression modeling in R.
* `modeling.R`: reads in table exported in `export-for-R.ipynb` and performs regression modeling relevant to RQ2.
* `add_drop_calendar_fa16_fa22.xlsx`: Academic calendar and phase reference dates by semester.

## EDM Extension Analysis Code

* `extended-analyses-aggregation.ipynb`: Generating data files for extension analyses
* `extended-analyses-growthcurves.R`: Growth curve models for causal inference
* `extended-analyses-modeling.ipynb`: Descriptives and modeling performed in Python, especially related to workload

