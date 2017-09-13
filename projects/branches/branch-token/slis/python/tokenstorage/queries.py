
# TODO: consider using "named style" in queries:

# Return one or zero records: non-activated token OR empty
# Parameter: {user_id}
_PICK_TOKEN_SQL_STMT = '''
SELECT * FROM Tokens 
WHERE user_id == {user_id} AND activation_date IS NULL AND expiry >= date("now")
LIMIT 1
'''

# Parameter: {user_id}
_COUNT_TOKENS_SQL_STMT = '''
SELECT Count() 
FROM Tokens 
WHERE activation_date IS NULL AND user_id == {user_id} AND expiry >= date("now")
'''

# Parameters: {id}, {user_id}
_ACTIVATE_TOKEN_SQL_STMT = '''
UPDATE Tokens 
SET activation_date=date("now") 
WHERE id == {id} AND user_id == {user_id}
'''

def acquireToken(db, user_id):
    '''
    Retrieve unused token belonging to user_id from the database db.

    Arguments
    ---------
    db: connection to sqlite3 SQL database
    user_id: id of user (owner of token)

    Returns
    -------
    Either: None if no token is available or error conditions were reached,
    OR
    tuple (expiry_date, key, token_data)
    '''
    try:
        with db:
            # Get either one row or None
            row = db.execute(_PICK_TOKEN_SQL_STMT.format(user_id=user_id)).fetchone()
            if row:
                # FIXME: this unpacking highly depends on the schema
                id, _, expiry, _, key, data = row
                db.execute(_ACTIVATE_TOKEN_SQL_STMT.format(id=id, user_id=user_id)).fetchone()
                return (expiry, key, data)
    except sqlite3.OperationalError:
        pass # Silence the error, but the function returns None



def getTokenCount(connection, user_id):
    cursor = connection.cursor()
    count = cursor.execute(_COUNT_TOKENS_SQL_STMT.format(user_id=user_id)).fetchone()
    return int(0 if count is None else count[0])

