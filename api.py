import json
from sanic import Sanic
from sanic.response import redirect
import socketio
import redis

sio = socketio.AsyncServer(async_mode='sanic', logger=True)
app = Sanic()
sio.attach(app)
r = redis.StrictRedis()
users = {}


@sio.on('connect')
async def connect(sid, environ):
    pass


@sio.on('disconnect')
async def disconnect(sid):
    users.pop(sid)


@sio.on('login')
async def login(sid, data):
    r.set('user:' + data['id'], json.dumps(data))
    users[sid] = data
    await full_sync(sid)
    await sio.emit('user', data)


@sio.on('update order')
async def update_order(sid, data):
    data['id'] = data.get('id', None) or get_next_order_id()
    r.set('order:{}'.format(data['id']), json.dumps(data))
    await sio.emit('order', data)


@sio.on('update position')
async def update_position(sid, data):
    user_id = users[sid]['id']
    data['participant'] = user_id
    r.set('position:{}:{}'.format(data['orderId'], user_id), json.dumps(data))
    await sio.emit('position', data)


async def full_sync(sid):
    orders = [json.loads(r.get(key)) for key in r.keys('order:*')]
    users = [json.loads(r.get(key)) for key in r.keys('user:*')]
    positions = [json.loads(r.get(key)) for key in r.keys('position:*')]
    print('sync', {
        'orders': orders,
        'users': users,
        'positions': positions,
    })
    await sio.emit('sync', {
        'orders': orders,
        'users': users,
        'positions': positions,
    },
        room=sid)


@app.route("/")
async def redirect_to_app(request):
    bare_host = request.host.split(':')[0]
    return redirect(f'http://{bare_host}:3000/')


_last_order_id = max(
    (int(k.split(b':')[-1])
     for k in r.keys(b'order:*')),
    default=0)


def get_next_order_id():
    global _last_order_id
    _last_order_id += 1
    return _last_order_id


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000)
