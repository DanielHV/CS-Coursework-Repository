import simpy
from Nodo import *
from Canales.CanalRecorridos import *
from random import randint

class NodoBroadcast(Nodo):
    ''' Implementa la interfaz de Nodo para el algoritmo de Broadcast.'''

    def __init__(self, id_nodo: int, vecinos: list, canal_entrada: simpy.Store,
                 canal_salida: simpy.Store):
        ''' 
        Constructor de NodoBroadcast.
        
        Args:
            id_nodo (int): Identificador del nodo.
            vecinos (list): Lista de nodos vecinos.
            canal_entrada (simpy.Store): Canal de entrada para recibir mensajes.
            canal_salida (simpy.Store): Canal de salida para enviar mensajes.
        '''
        super().__init__(id_nodo, vecinos, canal_entrada, canal_salida)
        self.mensaje = None
        self.reloj = 0
        self.eventos = []

    def broadcast(self, env: simpy.Environment, data="Mensaje"):
        ''' 
        Implementa el algoritmo de Broadcast para el nodo.
        
        Args:
            env (simpy.Environment): Entorno de simulación de SimPy.
            data (str): Mensaje a ser enviado. Por defecto es "Mensaje".
        '''
       
        if self.id_nodo == 0:
            
            # Mensaje inicial
            self.mensaje = data
            yield env.timeout(randint(1,5))
            
            for k in self.vecinos:
                self.reloj += 1
                self.eventos.append((self.reloj, "E", data, self.id_nodo, k))
                self.canal_salida.envia((data, self.reloj, self.id_nodo), [k])
            
        while True:
            # Espera un tiempo aleatorio entre 1 y 5 unidades de tiempo
            yield env.timeout(randint(1,5))
            # Recibe un mensaje desde el canal de entrada
            mensaje = yield self.canal_entrada.get()
            data = mensaje[0]
            reloj = mensaje[1]
            j = mensaje[2]
            
            # Asigna el mensaje recibido
            self.mensaje = data
            
            # Actualiza el reloj manteniendo el que tenga un tiempo mayor
            self.reloj = max(reloj, self.reloj) + 1
            self.eventos.append((self.reloj, "R", data, j, self.id_nodo))
            yield env.timeout(randint(1,5))
            
            for k in self.vecinos:
                self.reloj += 1
                self.eventos.append((self.reloj, "E", data, self.id_nodo, k))
                self.canal_salida.envia((data, self.reloj, self.id_nodo), [k])