from __future__ import print_function
import sys
import yaml
import json
import copy
import pyvef
import pyvef.configurators as pc
import pyvef.CustomFilters as CustomFilters


def main(input, output_prefix=None, input_texture=None, quiet=True,
         commands=True):

    try:
        root = yaml.load(file(input,'rt'))

        not quiet and print(json.dumps(root, indent=2))

    except yaml.YAMLError as e:
        errmsg = 'Problem with the input YAML file {file} (line {line}): {problem}'
        raise RuntimeError(errmsg.format(file=input,
                                         problem=e.problem, 
                                         line=e.problem_mark.line))

    generic = root['generic']
    jobs = root['jobs']

    if output_prefix is None:
        jobname_prefix = generic['output_prefix']
    else:
        jobname_prefix = output_prefix

    if input_texture is not None:
        generic['input_fname'] = input_texture

    # interpret 'jobs'. Make a list of (name, jobtype) tuples
    
    joblist = [ isinstance(job, dict) and job.items()[0] or (job, job) \
                for job in jobs]

    if not set([i for _, i in joblist]).issubset(set(pc.ALL_MODULES)):
        raise ValueError('incorrect module names found in job specification')

    #
    # Start processing
    #
    command_template = 'alamDMC {module_name} {config_file}\n'
    command_lines = []

    for jobname, module_name in joblist:

        try:
            if jobname not in root:
                raise ValueError('no configuration for job "{jobname}" in {input}'.format(**locals()))
            try:
                configurator = pc.CONFIGURATORS[module_name]
            except KeyError:
                raise  ValueError('job "{jobname}" has invalid type "{module_name}"'.format(**locals()))
            # Make job config
            job_config = copy.deepcopy(generic)
            # Populate the config
            module_jobname = jobname_prefix + '_' + jobname
            # update the output path
            job_config['output_prefix'] = module_jobname
            #
            # Prepare configuration dictionary for the module
            job_config[module_name] = root[jobname]
            # Fill in the template
            t = configurator(searchList=[job_config,()], filtersLib=CustomFilters)
        
            not quiet and print(t.output())
            # Write out the config
            config_file = module_jobname + '.cfg'
            with open(config_file, 'wt') as f:
                f.write(t.output())

            if commands:
                # Create command lines
                command_lines.append(command_template.format(config_file=config_file,
                                                             module_name=module_name.upper()))
        except ValueError as e:
            print('Warning: {}'.format(str(e)))

        not quiet and print('----')

    if command_lines:
        not quiet and print('\nHere are the commands you need to run the jobs '
                            'defined in {}: \n'.format(input))
        sys.stdout.writelines(command_lines + ['\n'])

if __name__ == '__main__':
    import argparse
    import textwrap
    #
    dsc = textwrap.dedent('''
        Generate alamDMC configuration files from YAML job specification

        The purpose of the tool is to run various types of VEF simulations for
        the same basic settings (texture data, hardening etc.).
        ''')

    parser = argparse.ArgumentParser(prog='vef_confgen',
                                     description=dsc, 
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--input',
                        required=True,
                        help='YAML job specification file')
    parser.add_argument('--output_prefix', 
                        default=None,
                        help='prefix for the output files. '
                             'It overrides the settings in the YAML file.'
                        )
    parser.add_argument('--input_texture',
                        default=None,
                        help='path to the input texture file (SMT). '
                             'Overrives the setting in the YAML file.')
    parser.add_argument('--quiet', 
                        default=False,
                        action='store_true',
                        help='suppress the terminal output')
    args = parser.parse_args()

    try:
        main(**vars(args))
    except RuntimeError as e:
        print('Error: {}'.format(e))
        sys.exit(1)
