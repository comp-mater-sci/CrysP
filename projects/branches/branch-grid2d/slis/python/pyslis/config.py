
import io
import os
import yaml


envvar = os.environ.get('SLIS_ROOT')
config_default = 'slis.yaml'


if envvar is None:
    config_path = ''
else:
    config_path = os.path.join(envvar, 'config', config_default)

config = yaml.load(io.open(config_path))

