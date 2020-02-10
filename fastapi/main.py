from fastapi import FastAPI
from pymongo import MongoClient
from starlette.middleware.cors import CORSMiddleware

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


@app.get("/")
def read_root():
    return {"Hello": "World"}


@app.get('/csm')
def csm():
    """List CSM"""

    coll = db['csm']
    f = lambda rec: {k: rec[k] for k in rec if k != '_id'}
    return list(map(f, coll.find()))
