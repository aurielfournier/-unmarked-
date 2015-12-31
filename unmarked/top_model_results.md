# 2012
```
Call:
gdistsamp(lambdaformula = ~region - 1, phiformula = ~1, pformula = ~1, 
    data = umf, keyfun = "hazard", output = "abund", mixture = "NB", 
    se = T)

Abundance:
         Estimate    SE     z  P(>|z|)
regionnc     4.69 0.252 18.59 3.88e-77
regionne     4.10 0.286 14.32 1.66e-46
regionnw     4.91 0.690  7.11 1.15e-12
regionse     3.44 0.255 13.49 1.77e-41

Availability:
 Estimate   SE     z P(>|z|)
     11.7 40.8 0.287   0.774

Detection:
 Estimate   SE     z P(>|z|)
  0.00522 0.05 0.104   0.917

Hazard-rate(scale):
 Estimate     SE    z  P(>|z|)
    0.682 0.0349 19.6 4.09e-85

Dispersion:
 Estimate    SE     z P(>|z|)
   0.0739 0.219 0.338   0.735

AIC: -495.895 
```

# 2013
```
Call:
gdistsamp(lambdaformula = ~region + scale_averagewater + region * 
    scale_averagewater - 1, phiformula = ~1, pformula = ~1, data = umf, 
    keyfun = "hazard", output = "abund", mixture = "NB", se = T)

Abundance:
                            Estimate    SE     z  P(>|z|)
regionnc                       3.191 0.247 12.93 3.11e-38
regionne                       3.894 0.429  9.08 1.13e-19
regionnw                       3.580 0.251 14.26 3.91e-46
regionse                       2.891 0.383  7.56 4.07e-14
scale_averagewater            -0.773 0.384 -2.01 4.43e-02
regionne:scale_averagewater    2.005 1.267  1.58 1.13e-01
regionnw:scale_averagewater    1.826 0.560  3.26 1.10e-03
regionse:scale_averagewater    2.155 0.706  3.05 2.26e-03

Availability:
 Estimate     SE     z   P(>|z|)
    -1.57 0.0618 -25.5 3.17e-143

Detection:
 Estimate  SE   z P(>|z|)
     4.14 NaN NaN     NaN

Hazard-rate(scale):
 Estimate   SE      z P(>|z|)
      -12 69.1 -0.174   0.862

Dispersion:
 Estimate    SE      z P(>|z|)
  -0.0062 0.172 -0.036   0.971

AIC: 4302.604 
```
# 2014
```
Call:
gdistsamp(lambdaformula = ~region + scale_averagewater - 1, phiformula = ~1, 
    pformula = ~1, data = umf, keyfun = "hazard", output = "abund", 
    mixture = "NB", se = T)

Abundance:
                   Estimate    SE    z  P(>|z|)
regionnc              2.093 0.230  9.1 9.44e-20
regionne              2.451 0.164 14.9 3.30e-50
regionnw              2.593 0.205 12.6 1.41e-36
regionse              2.147 0.194 11.1 2.08e-28
scale_averagewater    0.612 0.115  5.3 1.15e-07

Availability:
 Estimate     SE     z   P(>|z|)
    -1.57 0.0606 -25.8 3.52e-147

Detection:
 Estimate  SE   z P(>|z|)
     3.81 NaN NaN     NaN

Hazard-rate(scale):
 Estimate   SE      z P(>|z|)
    -12.6 42.8 -0.294   0.769

Dispersion:
 Estimate    SE     z P(>|z|)
    0.125 0.132 0.945   0.345

AIC: 4959.326 
```