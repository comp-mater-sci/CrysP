from __future__ import print_function
import yaml
import json
import pyvef
import pyvef.configurators as pc
from pyvef.configurators.configQRS import configQRS
from pyvef.configurators.configGeneric import configGeneric
from pyvef.configurators.configQRS import configQRS
from pyvef.configurators.configYLD import configYLD
from pyvef.configurators.configUDSA import configUDSA
from pyvef.configurators.configEWC import configEWC
from pyvef.configurators.configASR import configASR

import pyvef.CustomFilters as CustomFilters

_all_modules = ['qrs', 'yld', 'udsa', 'asr', 'ewc']

_module_configs = {'qrs': configQRS,
                  'yld': configYLD,
                  'udsa': configUDSA,
                  'ewc': configEWC,
                  'asr': configASR}

def main(input, modules):

    if 'all' in modules:
        modules = _all_modules

    data_source_generic = yaml.load(file(input,'rt'))

    print(json.dumps(data_source_generic, indent=2))

    data_source = data_source_generic['generic']

    jobname_prefix = data_source_generic['generic']['output_prefix']
                      
    for module_name in modules:
        configurator = _module_configs[module_name]
        jobname = jobname_prefix + '_' + module_name
        # update the output
        data_source['output_prefix'] = jobname
        # Plug in the data module source
        data_source[module_name] = data_source_generic[module_name]

        # print(data_source)

        t = configurator(searchList=[data_source,()], filtersLib=CustomFilters)
        
        print(t.output())
        
        with open(jobname + '.cfg', 'wt') as f:
            f.write(t.output())

        print('----')

if __name__ == '__main__':
    import argparse
    #
    dsc = ''''Generate alamDMC configuration files'''

    parser = argparse.ArgumentParser(description=dsc)
    parser.add_argument(
        '--input',
        required=True,
        help='YAML file')
    parser.add_argument(
        '--modules',
        choices=['all'] + _all_modules,
        nargs='+',
        default=['all'],
        help='Module configurations to be generated'
        )
    args = parser.parse_args()

    main(**vars(args))

