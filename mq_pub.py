from paho.mqtt import client
from paho import mqtt

import time
# The callback for when the client receives a CONNACK response from the server.
def on_connect(client, userdata, flags, rc):
    print("Connected with result code "+str(rc))
    time.sleep(2)
    client.publish("user/bob/location", "home", retain=True, qos=1)
    client.publish("user/bob/location", "work", retain=True, qos=1)

    # Subscribing in on_connect() means that if we lose the connection and
    # reconnect then subscriptions will be renewed.
    client.subscribe("user/bob/location")

# The callback for when a PUBLISH message is received from the server.
def on_message(client, userdata, msg):
    print(msg.topic+" "+str(msg.payload))
    time.sleep(2)

def on_log(*args):
    print args


client = client.Client()
client.on_connect = on_connect
client.on_message = on_message
client.on_log = on_log
client.username_pw_set("bob", password=None)
client.connect("localhost", 8000, 60)


# Blocking call that processes network traffic, dispatches callbacks and
# handles reconnecting.
# Other loop*() functions are available that give a threaded interface and a
# manual interface.
client.loop_forever()
