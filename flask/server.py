import json
from flask import Flask, jsonify
from pymongo import MongoClient

app = Flask(__name__)
client = MongoClient('mongodb://localhost:27017/')
db = client['uasrc']

@app.route('/')
def csm():
    """List CSM"""

    coll = db['csm']
    f = lambda rec: {k: rec[k] for k in rec if k != '_id'}
    data = list(map(f, coll.find()))
    return jsonify(data)
