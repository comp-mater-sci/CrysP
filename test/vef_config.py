MESO_CONFIG = {'FCTaylor': \
                    'FCTaylor', \
               'ALAMEL': \
                    'ALAMEL\n'  \
                    + 'equiaxed.smt'}

MODE_CONFIG =  {'ADP':                                              \
                    '3\n'                                           \
                    + '0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0\n'   \
                    + '0.2\n' \
                    + '1.0 1.0 0.0 0.0 -1.0 0.0 0.0 0.0 0.0\n'   \
                    + '0.2\n' \
                    + '1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 -1.0\n'   \
                    + '0.2', \
                'ASR':                                              \
                      '2\n'                                         \
                    + '1.0 1.0 0.0 0.0 0.0 0.0\n'                   \
                    + '0.25 \n'                      \
                    + '-1.0 -1.0 0.0 0.0 0.0 0.0\n'                 \
                    + '0.25',                            \
               'QRS':                                               \
                      '1.0',                                        \
               'YLD':                                               \
                      '36.0\n'                                      \
                    + 'True'}

HARDENING_CONFIG = {'NONE':                         \
                        '0',                        \
                    'VOCE':                         \
                        '1\n'                       \
                         + '12.39 15 20 0.2 0.1',   \
                    'HOCKETT_SHERBY':                         \
                        '2\n'                       \
                         + '1 151.2 29.79 0.4809',   \
                    'SWIFT':                        \
                        '3\n'                       \
                        + '12.39 1.e-3 0.24',       \
                    'DSH_EDGE':                           \
                        '11\n'                      \
                        + 'DSHparaset.txt',        \
                    'DSH_SCREW':                   \
                        '12\n'                      \
                        + 'DSHparaset.txt',        \
                    'DSH_LOOP':                    \
                        '13\n'                      \
                        + 'DSHparaset.txt'}

DSH_CONFIG = '2.48E-10\n'   \
             + '8.16E4\n'   \
             + '0.20\n'     \
             + '0.20\n'     \
             + '53.0\n'     \
             + '2.12E-2\n'  \
             + '8.89E-10\n' \
             + '8.37E-1\n'  \
             + '2.66E-8\n'  \
             + '2.27E-9\n'  \
             + '9.59\n'     \
             + '1.07\n'     \
             + '5.45E-2\n'  \
             + '1.44E-9\n'  \
             + '4.12E-9\n'  \
             + '8.70E-9'

def generate_config(mode, algorithm, slip_system, hardening_model):
    return 'out\n'                                      \
           + 'True\n'                                   \
           + '2\n'                                      \
           + 'texture.smt\n'                            \
           + MESO_CONFIG[algorithm] + '\n'                           \
           + slip_system + '\n'                         \
           + HARDENING_CONFIG[hardening_model] + '\n'   \
           + MODE_CONFIG[mode]
