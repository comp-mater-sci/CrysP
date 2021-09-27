
from __future__ import print_function

import requests
import urllib.parse # Python3 equivalent of urlparse

import pyslis.config

_DEFAULT_TIMEOUT = 5

def _get_tokens_api_url(config):
    try:
        api = urllib.parse.urljoin(config['host'], config['api_version']) + '/'
        request = urllib.parse.urljoin(api, config['user_key']) + '/tokens'
        return request
    except AttributeError as excpt:
        raise RuntimeError('Incorrect configuration')



def _get_token(config):
    try:
        timeout = config.get('timeout', _DEFAULT_TIMEOUT)
        request = _get_tokens_api_url(config)
    except AttributeError as excpt:
        raise RuntimeError('Incorrect configuration')
    try:

        req = requests.put(request, timeout=timeout)
        if req.status_code == requests.codes.ok:
            # Get json data, verify against content
            response = req.json()

            return response

    except requests.Timeout as excpt:
        print('Error: Cannot reach the token server')
        raise


def acquireToken(save_path):
    '''
    
    Returns
    -------
    True on success (ie token is acquired and properly written out), 
    False otherwise
    '''
    try:
        token_data = _get_token(pyslis.config.config).get('token_data', None)
        if token_data:
            with open(save_path, 'w') as outfile:
                outfile.write(token_data)
                return True
        return False
    except:
        pass

    return False

def getTokenCount():
    try:
        timeout = pyslis.config.config.get('timeout', _DEFAULT_TIMEOUT)
        url = _get_tokens_api_url(pyslis.config.config) + '/count'
        req = requests.get(url, timeout=timeout)
        if req.status_code == requests.codes.ok:
            return req.json()
    except requests.Timeout as excpt:
        print('Error: Cannot reach the token server within {timeout} seconds.'
              ' Try to increase timeout in slis.yaml configuration file.'
              ''.format(timeout=timeout))



