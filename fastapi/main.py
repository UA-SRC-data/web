import dateparser
import datetime
from fastapi import FastAPI
from pymongo import MongoClient
from starlette.middleware.cors import CORSMiddleware
from scrutinizer import Variable
from typing import Optional

app = FastAPI()
client = MongoClient('mongodb://localhost:27017/')
db = client['uasrc']

origins = [
    "http://localhost:*",
    "*",
]

app.add_middleware(
    CORSMiddleware,
    allow_origins=origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


# --------------------------------------------------
@app.get('/data/csm/variables')
def csm_measurements():
    """List CSM measurements"""

    # return list(
    #     map(lambda m: {'variable_name': m},
    #         sorted(db['csm'].distinct('variable_name'))))

    f = lambda rec: {k: rec[k] for k in rec if k != '_id'}
    return list(
        map(f, db['csm_variables'].find({}, {
            'name': 1,
            'desc': 1
        }).sort('name')))


# --------------------------------------------------
@app.get('/data/csm/locations')
def csm_stations():
    """List CSM locations/stations"""

    return list(
        map(lambda s: {'location_name': s},
            sorted(db['csm'].distinct('location_name'))))


# --------------------------------------------------
@app.get('/data/csm')
def csm(variable_name: str = '',
        location_name: str = '',
        start_date: str = '',
        end_date: str = '',
        val_max: Optional[float] = None,
        val_min: Optional[float] = None):
    """List CSM"""

    coll = db['csm']

    qry = {}
    prj = {
        'location_name': 1,
        'variable_name': 1,
        'collected_on': 1,
        'value': 1
    }

    if variable_name:
        qry['variable_name'] = variable_name

    if location_name:
        qry['location_name'] = location_name

    start_date = convert_date(start_date)
    end_date = convert_date(end_date)
    if start_date and end_date:
        qry['collected_on'] = {'$gte': start_date, '$lte': end_date}
    elif start_date:
        qry['collected_on'] = {'$gte': start_date}
    elif end_date:
        qry['collected_on'] = {'$lte': end_date}

    if val_max is not None and val_min is not None:
        qry['value'] = {'$gte': val_min, '$lte': val_max}
    elif val_max is not None:
        qry['value'] = {'$lte': val_max}
    elif val_min is not None:
        qry['value'] = {'$gte': val_min}

    f = lambda rec: {k: rec[k] for k in rec if k != '_id'}
    return list(map(f, coll.find(qry, prj)))


# --------------------------------------------------
def convert_date(date):
    """Convert a date"""

    if date:
        dt = dateparser.parse(date)
        if dt:
            return datetime.datetime.utcfromtimestamp(dt.timestamp())


# --------------------------------------------------
@app.get('/scrutinizer/variables')
def scrutinizer_variables():
    """List Scrutinizer variables"""

    f = lambda rec: {k: rec[k] for k in rec if k != '_id'}
    return list(
        map(
            f, db['variables'].find({}, {
                'name': 1,
                'desc': 1,
                'source': 1,
                'unit': 1
            }).sort('name')))


# --------------------------------------------------
@app.get('/scrutinizer/measurements')
def scrutinizer_measurements(variable: str = '',
                             location_name: str = '',
                             location_type: str = '',
                             max_value: float = None,
                             min_value: float = None,
                             start_date: str = '',
                             end_date: str = ''):
    """List Scrutinizer measurements"""

    coll = db['scrutinizer']
    qry = {}
    prj = {
        'location_name': 1,
        'location_type': 1,
        'variable_name': 1,
        'variable_desc': 1,
        'value': 1,
        'medium': 1,
        'collected_on': 1,
        'source': 1,
        'unit': 1
    }

    if variable:
        qry['variable_name'] = variable

    if location_name:
        qry['location_name'] = location_name

    if location_type:
        qry['location_type'] = location_type

    #print(f'max_value "{max_value}"')
    #print(f'min_value "{min_value}"')

    if max_value is not None and min_value is not None:
        qry['value'] = {'$gte': min_value, '$lte': max_value}
    elif max_value is not None:
        qry['value'] = {'$lte': max_value}
    elif min_value is not None:
        qry['value'] = {'$gte': min_value}

    start_date = convert_date(start_date)
    end_date = convert_date(end_date)
    if start_date and end_date:
        qry['collection_date'] = {'$gte': start_date, '$lte': end_date}
    elif start_date:
        qry['collection_date'] = {'$gte': start_date}
    elif end_date:
        qry['collection_date'] = {'$lte': end_date}

    def fix_id(rec):
        new = {k: rec[k] for k in rec if k != '_id'}
        new['id'] = str(rec.get('_id'))
        return new

    print(qry)
    return list(map(fix_id, coll.find(qry, prj)))
