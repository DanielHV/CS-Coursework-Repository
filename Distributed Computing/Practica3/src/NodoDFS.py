import simpy
from Nodo import *
from Canales.CanalRecorridos import *

# La unidad de tiempo
TICK = 1

class NodoDFS(Nodo):
    ''' 
    Implementa la interfaz de Nodo para el algoritmo de Broadcast utilizando DFS.
    
    Atributos:
        id_nodo (int): Identificador único del nodo.
        vecinos (list): Lista de nodos vecinos.
        canal_entrada (simpy.Store): Canal de entrada para recibir mensajes.
        canal_salida (CanalRecorridos): Canal de salida para enviar mensajes.
        padre (int): Identificador del nodo padre en el árbol DFS.
        hijos (list): Lista de identificadores de los nodos hijos en el árbol DFS.
    '''
    def __init__(self, id_nodo, vecinos, canal_entrada, canal_salida):
        '''
        Inicializa el nodo.
        
        Args:
            id_nodo (int): Identificador único del nodo.
            vecinos (list): Lista de nodos vecinos.
            canal_entrada (simpy.Store): Canal de entrada para recibir mensajes.
            canal_salida (CanalRecorridos): Canal de salida para enviar mensajes.
        '''
        # Tu implementación va aquí
        self.id_nodo = id_nodo
        self.vecinos = vecinos
        self.canal_entrada = canal_entrada
        self.canal_salida = canal_salida
        self.padre = id_nodo
        self.hijos = []

    def dfs(self, env):
        '''
        Algoritmo BFS.
        
        Args:
            env (simpy.Environment): El entorno de simulación de SimPy.
        
        Comportamiento:
            - Si el nodo es el nodo distinguido (id_nodo == 0), envía un mensaje "GO" al vecino de menor id (siendo el nodo distinguido el único visitado) y lo asigna como hijo.
            - Cuando un nodo recibe un mensaje "GO", si aún hay vecinos no visitados, se asigna el nodo que envió el mensaje como su padre y reenvía el mensaje "GO" al vecino no visitado de menor id y lo asigna como hijo, actualizando los nodos visitados.
            - Cuando un nodo recibe un mensaje "GO", Si todos los vecinos fueron visitados, envía un mensaje "BACK" a su padre actualizando los nodos visitados.
            - Cuando un nodo recibe un mensaje "BACK", si aún hay vecinos no visitados, envía "GO" al vecino de menor id sin actualizar los nodos visitados.
            - Cuando un nodo recibe un mensaje "BACK", si todos los vecinos fueron visitados, envía un mensaje "BACK" a su padre sin actualizar los nodos visitados.
            - Si el nodo distinguido recibe un mensaje "BACK", no hay más pasos por realizar.
        '''
        # Tu implementación va aquí

        # ------ estructura de mensajes: (id, GO, visitados), (id, BACK, visitados) ------
        
        # start
        if self.id_nodo == 0:
            siguiente = min(self.vecinos)
            self.canal_salida.envia((self.id_nodo, "GO", {self.id_nodo}), [siguiente]); self.hijos.append(siguiente)
            
        while True:
            mensaje = yield self.canal_entrada.get()
            
            if mensaje[1] == "GO":
                visitados = mensaje[2]
                
                self.padre = mensaje[0]
                if set(self.vecinos).issubset(visitados):
                    self.canal_salida.envia((self.id_nodo, "BACK", visitados | {self.id_nodo}), [mensaje[0]])
                else: 
                    siguiente = min(set(self.vecinos) - visitados)
                    self.canal_salida.envia((self.id_nodo, "GO", visitados | {self.id_nodo}), [siguiente]); self.hijos.append(siguiente)
            
            if mensaje[1] == "BACK":
                visitados = mensaje[2]
                
                if set(self.vecinos).issubset(visitados):
                    if self.padre != self.id_nodo:
                        self.canal_salida.envia((self.id_nodo, "BACK", visitados), [self.padre])
                else:
                    siguiente = min(set(self.vecinos) - visitados)
                    self.canal_salida.envia((self.id_nodo, "GO", visitados), [siguiente]); self.hijos.append(siguiente)