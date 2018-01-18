#!/bin/env python
# $Id$

'''Driver script of VEF material datacard generator'''

from __future__ import print_function

__version__ = '0.1.5' + '.' + '$Rev$'.strip('$Rev: ')

BACKEND_VERSION = '0.14.0'

import json
import csv
import uuid
import copy
import os
import subprocess
import multiprocessing
import tempfile
import sys
import datetime
from collections import OrderedDict
from itertools import chain
import numpy as np
import pyvef.configurators as pc
import pyvef.CustomFilters as CustomFilters


STRUCTURES = {'fcc': 'fcc12', 'bcc': 'bcc24'}


ALAMDMC_BIN = os.path.join(os.environ.get('VEF_ROOT'), 'bin', 'alamDMC')


VOIGT_3D = ['xx', 'yy', 'zz', 'xy', 'yz', 'xz']

VOIGT_2D = ['xx', 'yy', 'xy']


def as_voigt(X, dims):
    s = '{}_{{}}'.format(str(X))
    return [s.format(dim) for dim in dims]



def load_data(prefix, ext):
    outfile = '{}.{}'.format(prefix, ext)
    return np.genfromtxt(outfile, names=True, skip_header=1)


def transform_output(dataframe, key_field, fields):
    '''Transform multi-row dataframe (such as) into a dictionary where values
    of key_field become keys, while fields are set to the values.
    
    For a multi-row dataframe, create a dictionary with:
       - the value of key_field as the key, and
       -  dictionary with field,value pairs for the fields'''
    output = OrderedDict()
    for row in dataframe:
        key = row[key_field]
        output[key] = OrderedDict((field, row[field]) for field in fields)
    return output


def flatten_onelevel_dict(data, output_type=OrderedDict):
    '''Convert nested structure into a one-level dictionary'''
    output = output_type()
    for key, values in data.items():
        for fieldname, value in values.items():
            colname = '{field}_{key}'.format(field=fieldname, 
                                             key=key)
            output[colname] = value
    return output

# TODO: fix the design flaw: harvesters make assumption (use of implied
#       knowledge) about the module that provides the data. This knowledge
#       is explicitly present elsewhere. What's worse, it has to assume what
#       data (shape, type, ...) is generated. It violates the general policy:
#       as few assumptions as possible.

def uniaxial_setup(config, data):
    data = list(float(x) for x in data)
    module_name = 'qrs'
    module_config = {'range': {'type': 'discrete',
                               'params': {'values': data}},
                     'use_default_settings': True}
    return module_name, module_config, {}


def uniaxial_harvester(output_prefix):
    fields = ('rvalue', 'svalue', 'sigma_xx')
    dataframe = load_data(output_prefix, 'xqrs')
    return {'uniaxial': transform_output(dataframe, 'angle', fields)}


def equibiaxial_setup(config, data):
    module_name = 'udsa'
    module_config = {
        'sample_orientation': {'orientation': 'ND'},
        'scaling': {'type': 'StrainTensorComponent',
                    'step_magnitude':  0.01,
                    'increment_magnitude': 0.01},
        'stress_state': 'compression',
        'use_default_settings': True
        }
    return module_name, module_config, {}


def equibiaxial_harvester(output_prefix):
    fields = OrderedDict([('rvalue', 'r_bx'),
                          ('svalue', 's_bx'),
                          ('sigma_xx', 'sigma_bx')])
    output = {}
    # Consider only the first data line
    point = load_data(output_prefix, 'uds')[0]
    # tweak: point['sigma_xx'] is negative, but it should be reported positive
    point['sigma_xx'] *= -1.
    return {'equibiaxial': OrderedDict((name, point[field]) \
            for field, name in fields.iteritems())}



    # Alternate implementation of 'inplane' would make use of the yld module
    #    data_name = 'inplane'
    #    module_name = 'yld'
    #    # basic config
    #    module_config = {
    #        'theta_range': {'type': 'discrete',
    #                        'params': None # provisional
    #                        },
    #        'use_default_settings': False,
    #        'section':{'normalize_base': True,
    #                'base_sigma_x': [1., 0., 0., 0., 0., 0.],
    #                'base_sigma_y': [0., 1., 0., 0., 0., 0.],
    #                'base_sigma_offset': [0., 0., 0., 1., 0., 0.]},
    #        'normalize_stress': False,
    #        'offset_range': {'type': 'discrete', 'params': None},
    #        'use_scaling': False
    #        }
    #    for i, point in enumerate(inplane):
    #        job_config = make_config(jobid, module_name, config, 
    #                                 module_config,
    #                                 sequence=i)
    #        # update the config
    #        sigma_xx, sigma_yy, sigma_xy = (float(x) for x in point)
    #        theta = np.degrees(np.arctan2(sigma_yy, sigma_xx))
    #        cfg = job_config[module_name]
    #        cfg['theta_range']['params'] = {'values': [theta]}
    #        cfg['offset_range']['params'] = {'values': [sigma_xy]}
    #        jobs.append((data_name, module_name, job_config))


def inplane_setup(config, data):
    data = list(float(x) for x in data)
    IDX = np.array((0, 1, 3))
    full_stress = np.zeros(6)
    full_stress[IDX] = np.array(data)
    inputs = OrderedDict(zip(as_voigt('sigma', VOIGT_2D), data))
    setup = arbitrary_setup(config, full_stress)
    return setup[0], setup[1], inputs


def inplane_harvester(output_prefix):
    fields = ('scal_s', 'S', 'A_xx', 'A_yy', 'A_zz', 'A_xy')
    point = load_data(output_prefix, 'asr')[0]
    return {'inplane': OrderedDict((field, point[field]) for field in fields)}


def arbitrary_setup(config, data):
    data = list(float(x) for x in data)
    module_name = 'asr'
    module_config = {
        'reference_frame': [0., 0., 0.],
        # This should be: 
        # 'steps': [{'stress_mode': data, 'update_state': False}]
        # but because of a bug in alamDMC incrementation control,
        # we need to run an evolution step...
        'steps': [
            {'stress_mode': data,
             'update_state': True,
             'scaling': {'type': 'StrainTensor',
                         'step_magnitude':  0.01,
                         'increment_magnitude': 0.01}
             }
            ]
        }
    inputs = OrderedDict(zip(as_voigt('sigma', VOIGT_3D), data))
    return module_name, module_config, inputs


def arbitrary_harvester(output_prefix):
    fields = ('scal_s', 'S', 'A_xx', 'A_yy', 'A_zz', 'A_xy', 'A_yz', 'A_xz')
    point = load_data(output_prefix, 'asr')[0]
    return {'arbitrary': OrderedDict((field, point[field]) for field in fields)}


def yld_setup(config, data):
    begin,end,step = list(float(x) for x in data)
    module_name = 'yld'
    # Special treatment to make sure that normals to the yield locus are
    # properly estimated. In principle, this ought to be taken care of by the
    # backend, but at the moment it is not.
    # We add auxiliary points using the central diffetence stencil with delta:
    delta = 0.5
    discrete = np.append(np.arange(begin, end, step, dtype=float), [end])

    discrete_ext = list(chain(*[(x-delta, x, x+delta) for x in discrete]))

    module_config = {
        'theta_range': {'type': 'discrete',
                        'params': {'values': discrete_ext}},
        'use_default_settings': True
        }
    return module_name, module_config, {}



def yld_harvester(output_prefix):
    dataframe = load_data(output_prefix, 'xyld')
    key_field = 'theta'
    fields = ('sigma', 'sigma_scaled', 'S', 'dotW', 'sigma_x', 'sigma_y', 'dsigma_x', 'dsigma_y', 'beta')
    # Special treatment: filter out the auxiliary points
    return {'yld': transform_output(dataframe[1::3], key_field, fields)}



CONFIGURATORS = {'uniaxial': uniaxial_setup,
                 'equibiaxial': equibiaxial_setup,
                 'inplane': inplane_setup,
                 'arbitrary': arbitrary_setup,
                 'yld': yld_setup}

HARVESTERS = {'uniaxial': uniaxial_harvester,
              'equibiaxial': equibiaxial_harvester,
              'inplane': inplane_harvester,
              'arbitrary': arbitrary_harvester,
              'yld': yld_harvester}

FLATTENERS = {'uniaxial': flatten_onelevel_dict,
              'yld': flatten_onelevel_dict}

def output_csv(jobname, data, meta, transform_map=None, **kwargs):

    # transform data into a dictionary
    results = OrderedDict([('jobname', jobname),
                           ('jobid', meta['id']),
                           ('structure', meta['material']['structure'])])
    
    tr = {} if transform_map is None else transform_map
    
    # TODO: we need a more robust method of resolving duplicate names.
    #       This is issue is really painful for 'inplane' and 'arbitrary'.
    for item in data:
        provider = item.keys()[0]
        result = item.values()[0]
        tmp = tr[provider](result) if tr.get(provider) else result
        results.update(tmp)


    with file(jobname + '.csv', 'wb') as f:
        writer = csv.DictWriter(f, results.keys())
        writer.writeheader()
        writer.writerow(results)



def run_job(job, workdir='', keep_intermediate=True):
    data_component, provider, config = job

    config['output_prefix'] = os.path.join(workdir, config['output_prefix'])
    output_prefix = config['output_prefix']

    configurator = pc.CONFIGURATORS[provider]
    template = configurator(searchList=[config,()], filtersLib=CustomFilters)

    # Write out config file
    cfg_path = os.path.join(workdir, '{}.cfg'.format(output_prefix))
    try:
        with open(cfg_path, 'wt') as f:
            f.write(template.output())

        # Run the alamDMC
        command = [ALAMDMC_BIN, provider.upper(), cfg_path]
        subprocess.check_call(command,
                              stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE)
        # Collect the results
        return HARVESTERS[data_component](output_prefix)
            
    except subprocess.CalledProcessError as e:
        print(e)
        raise
    
    return None


def make_config(id, module_name, config, module_config, sequence=None):
    job_config=copy.deepcopy(config)
    seq='' if sequence is None else '_{}'.format(sequence)
    job_config['output_prefix'] += '_{}_{}{}'.format(module_name, id, seq)
    job_config[module_name] = module_config
    return job_config


def main(jobname, input, cpmodel, structure, serial, intermediate_dir,
         as_json=True, as_csv=True, **requests):

    try:

        jobid = str(uuid.uuid1())
        if intermediate_dir:
            workdir = os.path.abspath(os.path.join(intermediate_dir,jobid))
            try:
                os.makedirs(workdir)
            except os.error:
                pass
        else:
            workdir = tempfile.mkdtemp()

        #
        # prepare the generic section
        #
        config = {
            'output_prefix': os.path.join(workdir,jobname),
            'verbosity': 0,
            'output_request': False,
            'input_fname': os.path.abspath(input),
            'model_type': cpmodel,
            'slipsystems': STRUCTURES[structure],
            'use_default_microstructure': True,
            'use_default_hardening': True,
            'use_default_solver_settings': True
            }
        #
        # 
        #

        meta = {'id': jobid,
                'generator': {'name': 'vef_datacard',
                              'version': __version__,
                              'backend': {'name': 'VEF',
                                          'version': BACKEND_VERSION}},
                'timestamp': str(datetime.datetime.now()),
                'material': {'structure': structure,
                             'data': os.path.basename(input)}
               }


        jobs = []

        # temporary nasty hack: configurators return 3rd field "inputs"
        inputs = []

        for request in requests.items():
            try:
                name, data_list = request
                for data in data_list:
                    if not data:
                        continue
                    job_seq = '{}_{}'.format(jobid, len(jobs))
                    module_name, module_config, module_input = CONFIGURATORS[name](config, data)
                    job_config = make_config(job_seq, module_name, config, 
                                             module_config)
                    jobs.append((name, module_name, job_config))
                    inputs.append(module_input)
            except KeyError:
                print('Error: unknown request {}'.format(request))

        if serial:
            results = [run_job(job) for job in jobs]
        else:
            pool = multiprocessing.Pool()
            results = pool.map(run_job, jobs)

        pass
        # temporary nasty hack: merge inputs and results
        for input, result in zip(inputs, results):
            if input:
                source = result.keys()[0]
                result[source]['input'] = input

        output = {'meta': meta, 'data': results}

        #
        # Present the results
        #
        if as_json:
            with file(jobname+'.json','wt') as f:
                json.dump(output, f, indent=2)

        if as_csv:
            output_csv(jobname, transform_map=FLATTENERS, **output)
                
        return 0

    except Exception as e:
        print('A general exception has occurred. '
              'This is unusual, so please report that to the developers.\n')
        e.message and print('More detail:', e.message)
        return 2

if __name__ == '__main__':
    import argparse
    import textwrap
    #
    dsc = textwrap.dedent('''
        Calculate datacard for yield locus calibration

        The purpose of the tool is to calculate at once various yield points 
        typically used in calibration of advanced yield locus models.
    ''')

    parser = argparse.ArgumentParser(prog='vef_datacard', description=dsc)

    parser.add_argument('--jobname',
                        default='vef_datacard',
                        help='Name to be used as prefix of the output files')

    parser.add_argument('--input',
                        required=True,
                        help='Texture file')

    parser.add_argument('--cpmodel',
                        choices=['ALAMEL', 'FCTaylor'],
                        default='ALAMEL',
                        help='Crystal plasticity model to be used')

    parser.add_argument('--structure',
                        required=True,
                        choices=['fcc', 'bcc'])

    parser.add_argument('--uniaxial',
                        nargs='+',
                        action='append',
                        default=[],
                        help='Directions of uniaxial tension tests')

    parser.add_argument('--equibiaxial',
                        action='append_const',
                        const=True,
                        default=[],
                        help='Evaluate equibiaxial tension point')

    inplane_help = '''Evaluate in-plane stress point givend by:
    sigma_xx sigma_yy sigma_xy'''
    parser.add_argument('--inplane',
                        nargs=3,
                        metavar=('sigma_xx', 'sigma_yy', 'sigma_xy'),
                        action='append',
                        default=[],
                        help=inplane_help)

    arbitrary_help = '''\
    Evaluate an arbirtary stress point specified in Voce convention:
    sigma_xx sigma_yy sigma_zz sigma_xy sigma_yz sigma_xz'''
    parser.add_argument('--arbitrary',
                        nargs=6,
                        metavar=('sigma_xx', 'sigma_yy', 'sigma_zz', 
                                 'sigma_xy', 'sigma_yz', 'sigma_xz'),
                        action='append',
                        default=[],
                        help=arbitrary_help)

    parser.add_argument('--yld',
                        nargs=3,
                        metavar=('theta_begin', 'theta_end', 'theta_step'),
                        action='append',
                        default=[],
                        help='Calculate yield locus characteristics')
    #parser.add_argument('--json',
    #                    default=False,
    #                    action='store_true',
    #                    dest='as_json',
    #                    help='Output in JSON format')

    parser.add_argument('--version', action='version', 
                        version='%(prog)s {}'.format(__version__))

    parser.add_argument('--serial', 
                        action='store_true',
                        default=False,
                        help='Force serial execution of the workflow')

    parser.add_argument('--intermediate_dir',
                        metavar='directory_path',
                        default=None,
                        help='Working directory where intermediate results are produced')


    args = parser.parse_args()

    DATASOURCES = ['uniaxial', 'equibiaxial', 'inplane', 'arbitrary', 'yld']
    switches = vars(args)

    if not any(switches[key] for key in DATASOURCES):
        print('You must request at least one of:', ' '.join(DATASOURCES))
        sys.exit(1)

    sys.exit(main(**vars(args)))

