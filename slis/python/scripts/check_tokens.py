
from __future__ import print_function



if __name__ == '__main__':
    import pyslis

    try:
        import pyslis.config
    except:
        print('Cannot load configuration')

    try:
        result = pyslis.getTokenCount()

        if result:
            print('Total number of tokens: {total}\n'
                  'Available tokens: {available}'.format(**result))

    except KeyError:
        raise RuntimeError('Cannot interprete the response from token server')

