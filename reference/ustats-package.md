# ustats: R Interface to Python Tools for Computing Higher-Order U-Statistics

Provides an R interface to the Python package 'u-stats'
<https://pypi.org/project/u-stats/> for efficient computation of
higher-order U-statistics using Einstein summation notation,
implementing the methods of Chen, Zhang, and Liu (2025)
[doi:10.48550/arXiv.2508.12627](https://doi.org/10.48550/arXiv.2508.12627)
. The package automatically converts R objects to 'NumPy' or 'PyTorch'
tensors via 'reticulate' and supports GPU acceleration when 'PyTorch'
with 'CUDA' is available. Python dependencies are declared via
'reticulate' and can be installed automatically on first use. Designed
for large-scale statistical estimation where numerical stability and
performance are critical.

## See also

Useful links:

- <https://cxy0714.github.io/U-Statistics-R/>

- <https://github.com/cxy0714/U-Statistics-R>

- <https://pypi.org/project/u-stats/>

- Report bugs at <https://github.com/cxy0714/U-Statistics-R/issues>

## Author

**Maintainer**: Xingyu Chen <xingyuchen0714@sjtu.edu.cn>

Authors:

- Ruiqi Zhang <zrq1706@outlook.com>

- Lin Liu <linliu@sjtu.edu.cn>
