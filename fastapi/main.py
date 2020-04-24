import dateparser
import datetime
from fastapi import FastAPI
from pymongo import MongoClient
from starlette.middleware.cors import CORSMiddleware
from scrutinizer import Variable

#app = FastAPI()
app = FastAPI(openapi_prefix="/api/v1")
client = MongoClient('mongodb://localhost:27017/')
db = client['uasrc']

origins = [
    "http://localhost:*",
    "http://src.cals.arizona.edu:*",
    "*",
]

app.add_middleware(
    CORSMiddleware,
    allow_origins=origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

#app.mount("/api/v1", subapi)


# --------------------------------------------------
@app.get("/")
def read_root():
    return {"Hello": "World"}


# --------------------------------------------------
@app.get('/data/csm/measurements')
def csm_measurements():
    """List CSM measurements"""

    return list(
        map(lambda m: {'measurement': m},
            sorted(db['csm'].distinct('measurement'))))


# --------------------------------------------------
@app.get('/data/csm/stations')
def csm_stations():
    """List CSM stations"""

    return list(
        map(lambda s: {'station': s}, sorted(db['csm'].distinct('station'))))


# --------------------------------------------------
@app.get('/data/csm')
def csm(measurement: str = '',
        station: str = '',
        start_date: str = '',
        end_date: str = '',
        val_max: float = None,
        val_min: float = None):
    """List CSM"""

    coll = db['csm']

    qry = {}
    prj = {'station': 1, 'measurement': 1, 'collection_date': 1, 'val': 1}

    if measurement:
        qry['measurement'] = measurement

    if station:
        qry['station'] = station

    start_date = convert_date(start_date)
    end_date = convert_date(end_date)
    if start_date and end_date:
        qry['collection_date'] = {'$gte': start_date, '$lte': end_date}
    elif start_date:
        qry['collection_date'] = {'$gte': start_date}
    elif end_date:
        qry['collection_date'] = {'$lte': end_date}

    if val_max is not None and val_min is not None:
        qry['val'] = {'$gte': val_min, '$lte': val_max}
    elif val_max is not None:
        qry['val'] = {'$lte': val_max}
    elif val_min is not None:
        qry['val'] = {'$gte': val_min}

    f = lambda rec: {k: rec[k] for k in rec if k != '_id'}
    return list(map(f, coll.find(qry, prj)))


# --------------------------------------------------
@app.get('/scrutinizer/variables')
def scrutinizer_variables():
    """List Scrutinizer variables"""

    # return list(
    #     map(lambda v: {'variable': v},
    #         sorted(db['scrutinizer'].distinct('variable'))))

    qry = Variable.select()

    return [{
        'variable': v.variable,
        'description': v.description
    } for v in qry]


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
        'variable': 1,
        'value': 1,
        'medium': 1,
        'collected_on': 1
    }

    if variable:
        qry['variable'] = variable

    if location_name:
        qry['location_name'] = location_name

    if location_type:
        qry['location_type'] = location_type

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

    #print(qry)
    return list(map(fix_id, coll.find(qry, prj)))

# --------------------------------------------------
def convert_date(date):
    """Convert a date"""

    if date:
        dt = dateparser.parse(date)
        if dt:
            return datetime.datetime.utcfromtimestamp(dt.timestamp())


# @app.get('/data/census')
# def csm():
#     """List CSM"""

#     coll = db['csm']
#     f = lambda rec: {k: rec[k] for k in rec if k != '_id'}
#     return list(map(f, coll.find()))


