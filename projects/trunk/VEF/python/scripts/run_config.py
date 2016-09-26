from __future__ import print_function
import yaml
import json
import pyvef
import pyvef.configurators as pc
import pyvef.CustomFilters as CustomFilters


def main(input, modules):

    if 'all' in modules:
        modules = pc.ALL_MODULES

    data_source_generic = yaml.load(file(input,'rt'))

    print(json.dumps(data_source_generic, indent=2))

    data_source = data_source_generic['generic']

    jobname_prefix = data_source_generic['generic']['output_prefix']
                      
    for module_name in modules:
        configurator = pc.CONFIGURATORS[module_name]
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
        choices=['all'] + pc.ALL_MODULES,
        nargs='+',
        default=['all'],
        help='Module configurations to be generated'
        )
    args = parser.parse_args()

    main(**vars(args))

