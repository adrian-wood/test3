
MetDB Retrieval Regression Testing
==================================

See https://metnet2/content/metdb-technote-18a-regression-test-retrievals 

Installation Instructions
-------------------------

Get the latest version of the code from the metdb-misc repo in BitBucket
as user moodsf copy code as follows:
```
cd ~/metdb-misc/regression_testing
cp cylc/* /home/moodsf/cylc-run/regression/.
cp display/html/* /var/www/html/regression/.
cp display/scripts/* /var/www/cgi-bin/regression/.
cp -R runner/* /var/moods/regression/.
cp TestPlan.txt /var/moods/regression/.
```
Re-build executables:
```
cd /var/moods/regression/source
make
```

You may need to reload the cylc suite:
```
cylc reload regression
```
Integration Manager sets the versions to be tested each month following the preprod release:
```
cd cylc-run/regression
```
Edit set_env.rc and assign V1 to the new version to be tested and V2 to the current operational version:
```
 VER1=5.21.0
 VER2=5.20.0
```
Reload the cylc suite to pick up the changes:
```
cylc reload regression
```
Suite runs at 0630Z daily.

Output is http://mdb-apps-test/regression/latest_regression_tests.html

