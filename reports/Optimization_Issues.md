Optimization Issues
================

\[
  \Psi(\lambda, h, g) = \frac{1}{x_{max}-x_{min}} ( \sum_x [\ln\frac{1}{1-q_x} - \frac{h}{g}e^{gx}(e^g-1)-\lambda ] )^2
\]

For simplicity, Set \[
  R = x_{max}-x_{min}  \\
  Q = \sum_x \ln\frac{1}{1-q_x} \\
  H = \frac{h}{g}(e^g-1)
\]

So, \[
  \Psi(\lambda, h, g) = \frac{1}{R} ( Q - R\lambda - H\sum_x e^{gx} )^2
\]

Note that \(\lambda\) here has minimal interaction with any of the
obesrvations. It’s actually quite trivial to show that to minimize
\(\Psi\) on \(\lambda\) we set \(\lambda\) to minimum of its range (i.e.
\(\lambda \rightarrow 0\)) Additionally, if we used equation (10):
