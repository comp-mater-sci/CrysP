
from io import open

def acquireToken(save_path):
    '''Dummy 
    
    Returns
    -------
    True on success (ie token is acquired and properly written out), 
    False otherwise
    '''
    token_data = '''\
22 serialization::archive 15 0 0 0 0 0 0 5 1973393438 2108117442 131935254 2725919309 1451463436 0 0 c6119329-7c09-47ec-9e0b-9a7602543c30 7 token_1 32 Mitsubishi Materials Corporation 10 2019-09-01 0 0
'''

    with open(save_path, 'w') as outfile:
        outfile.write(token_data)
        return True

    return False

