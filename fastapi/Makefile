dev:
	uvicorn main:app --reload
	#gunicorn main:app --reload --config=config.py -k uvicorn.workers.UvicornWorker

live:
	#uvicorn main:app --port 8000 --workers 5 --log-level info >log &
	gunicorn main:app --config=config.py -k uvicorn.workers.UvicornWorker &

install:
	python3 -m pip install -r requirements.txt
