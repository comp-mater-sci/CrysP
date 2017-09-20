"""
This script runs the application using a development server.
It contains the definition of routes and views for the application.
"""

__author__ = 'Jerzy Gawad'
__copyright__ = 'KU Leuven'
__status__ = 'Prototype'

import re
from flask import Flask, request
from flask_restful import reqparse, abort, Api, Resource

import tokenstorage
import tokenstorage.queries as queries

db = tokenstorage.connect('database.db')

app = Flask(__name__)
api = Api(app, catch_all_404s=True)

#
# Validation of user_key
#
USER_KEY_LEN = 64
USER_KEY_PATTERN = re.compile('^[a-z,0-9]{64}$')


def filter_users(user_key):
    '''Verify that user_key is of proper format and identifies a valid user.
    '''
    # Check user_key format:
    if len(user_key) != USER_KEY_LEN or \
       USER_KEY_PATTERN.match(user_key) is None:
        abort(404, message='Invalid account')
    if queries.getUserId(db, user_key) is None:
        abort(404, message='Account does not exist')



class UsersTokens(Resource):

    def put(self, user_key):
        _put_log_message = 'PUT: Serving resource for {user_key} on {remote_addr}'
        print(_put_log_message.format(user_key=user_key,remote_addr=request.remote_addr))

        filter_users(user_key)
        user_id = queries.getUserId(db, user_key)
        record = queries.acquireToken(db, user_id)
        if record is None:
            abort(404, message='All tokens have been used')
        else:
            fields = ('token_expiry', 'token_key', 'token_data')
            return dict(zip(fields, record))


class UsersTokenCount(Resource):
    def get(self, user_key):
        _get_log_message = 'GET: Serving Count resource for {user_key} on {remote_addr}'
        print(_get_log_message.format(user_key=user_key,remote_addr=request.remote_addr))
        filter_users(user_key)
        # dummy
        user_id = queries.getUserId(db, user_key)
        # query database: total, available
        total = queries.getTokenCount(db, user_id, True)
        available = queries.getTokenCount(db, user_id)
        return {'total': total, 'available': available}

api.add_resource(UsersTokens, '/v1/<string:user_key>/tokens')
api.add_resource(UsersTokenCount, '/v1/<string:user_key>/tokens/count')

# Make the WSGI interface available at the top level so wfastcgi can get it.
wsgi_app = app.wsgi_app

if __name__ == '__main__':
    
    debug = True

    if debug:
        app.config['DEBUG']=True
        print('App config:')
        for key, value in app.config.items():
            print(key, value)

        app.run('localhost', 5555, debug=True)

    else:

        import os
        HOST = os.environ.get('SERVER_HOST', 'localhost')
        try:
            PORT = int(os.environ.get('SERVER_PORT', '5555'))
        except ValueError:
            PORT = 5555
        app.run(HOST, PORT)
