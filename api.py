from aiohttp import web
import json
import socketio
import sys
import redis

sio = socketio.AsyncServer(async_mode='aiohttp', logger=True)
app = web.Application()
routes = web.RouteTableDef()
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
    await sio.emit('order', upgrade_order(data))


@sio.on('update position')
async def update_position(sid, data):
    user_id = users[sid]['id']
    data['participant'] = user_id
    r.set('position:{}:{}'.format(data['orderId'], user_id), json.dumps(data))
    await sio.emit('position', data)


async def full_sync(sid):
    orders = [upgrade_order(json.loads(r.get(key)))
              for key in r.keys('order:*')]
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


@routes.get("/")
async def redirect_to_app(request):
    bare_host = request.host.split(':')[0]
    return web.HTTPFound(f'http://{bare_host}:3000/')


@routes.get("/admin")
async def admin_form(request, *, message=""):
    return web.Response(
        content_type="text/html",
        charset="utf-8",
        body=f"""
            <html><body><form method="post">
            {message}
            <button name="action" value="flush">Usuń wszystko</button>
            </form></body></html>""")


@routes.post("/admin")
async def admin_action(request):
    action = (await request.post())['action']
    if action == "flush":
        await flush_data()
        return await admin_form(request, message="Baza danych została wyczyszczona")
    else:
        return await admin_form(request, message=f"Nieznana komenda {action}")


async def flush_data():
    r.flushall()
    for sid in list(users.keys()):
        await full_sync(sid)

_last_order_id = max(
    (int(k.split(b':')[-1])
     for k in r.keys(b'order:*')),
    default=0)


def get_next_order_id():
    global _last_order_id
    _last_order_id += 1
    return _last_order_id


def upgrade_order(order: dict):
    order.setdefault('isOrdered', False)
    return order


async def on_shutdown(app):
    print("Bye bye")
    sys.exit(0)

app.on_shutdown.append(on_shutdown)

app.router.add_routes(routes)


if __name__ == "__main__":
    web.run_app(app, host="0.0.0.0", port=5000,
                handle_signals=True)
