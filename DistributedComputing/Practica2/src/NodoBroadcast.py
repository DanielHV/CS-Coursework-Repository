import simpy
import time
from Nodo import *
from Canales.CanalBroadcast import *

# La unidad de tiempo
TICK = 1

class NodoBroadcast(Nodo):
    ''' 
    Implementa la interfaz de Nodo para el algoritmo de Broadcast.
    
    Atributos:
        id_nodo (int): Identificador único del nodo.
        vecinos (list): Lista de nodos vecinos.
        canal_entrada (simpy.Store): Canal de entrada para recibir mensajes.
        canal_salida (CanalBroadcast): Canal de salida para enviar mensajes.
        mensaje (str): Mensaje a ser difundido.
    '''

    def __init__(self, id_nodo, vecinos, canal_entrada, canal_salida, mensaje=None):
        '''
        Inicializa el nodo con su identificador, vecinos y canales de comunicación.
        
        Args:
            id_nodo (int): Identificador único del nodo.
            vecinos (list): Lista de nodos vecinos.
            canal_entrada (simpy.Store): Canal de entrada para recibir mensajes.
            canal_salida (CanalBroadcast): Canal de salida para enviar mensajes.
            mensaje (str, opcional): Mensaje a ser difundido. Por defecto es None.
        '''
        self.id_nodo = id_nodo
        self.vecinos = vecinos
        self.canal_entrada = canal_entrada
        self.canal_salida = canal_salida
        self.mensaje = mensaje
            
    def broadcast(self, env):
        
        '''
        Algoritmo de Broadcast. Desde el nodo distinguido (0) vamos a enviar un mensaje a todos los demás nodos.
        
        Args:
            env (simpy.Environment): El entorno de simulación de SimPy.
        
        Comportamiento:
            - Si el nodo es el nodo distinguido (id_nodo == 0), envía el mensaje inicial a todos sus vecinos.
            - Cada nodo espera recibir un mensaje en su canal de entrada y lo reenvía a todos sus vecinos.
        '''
        
        if self.id_nodo == 0:
            self.mensaje = "saquen el lol"
            self.canal_salida.envia(self.mensaje, self.vecinos)
        
        while True:
            mensaje = yield self.canal_entrada.get()
            self.mensaje = mensaje
            self.canal_salida.envia(mensaje, self.vecinos)
        
        
