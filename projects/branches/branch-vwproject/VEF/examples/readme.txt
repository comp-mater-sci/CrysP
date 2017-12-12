To execute the examples, run `run_examples.cmd`

To re-make the example configuration files:

    vef_confgen --input confgen_sid1687f.yaml
    vef_confgen --input confgen_AKDQ.yaml

To re-make the examples and execute, you may use:

    for /f "tokens=*" %f in ('vef_confgen --input confgen_sid1687f.yaml --quiet') do %f

    for /f "tokens=*" %f in ('vef_confgen --input confgen_AKDQ.yaml --quiet') do %f
