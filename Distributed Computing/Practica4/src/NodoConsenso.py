import simpy
from Nodo import *
from Canales.CanalRecorridos import *

# La unidad de tiempo
TICK = 1

class NodoConsenso(Nodo):
    ''' Implementa la interfaz de Nodo para el algoritmo de Consenso.'''

    def __init__(self, id_nodo, vecinos, canal_entrada, canal_salida):
        ''' Constructor de nodo que implemente el algoritmo de consenso. '''
        self.id_nodo = id_nodo
        self.vecinos = vecinos
        self.canal_entrada = canal_entrada
        self.canal_salida = canal_salida
        # Atributos extra
        self.V = [None] * (len(vecinos) + 1) # Llenamos la lista de Nodos
        self.V[id_nodo] = id_nodo
        self.New = set([id_nodo])
        self.rec_from = [None] * (len(vecinos) + 1)
        self.fallare = False      # Colocaremos esta en True si el nodo fallará
        self.lider = None         # La elección del lider.

    def consenso(self, env, f):
        '''El algoritmo de consenso.'''
        # Aquí va su implementación
        
        if self.id_nodo in range(f):
            self.fallare = True
        
        while env.now <= f+1:
            
            if self.fallare: break
            
            yield env.timeout(TICK)
            
            if self.New != set():
                self.canal_salida.envia((self.id_nodo, self.New),self.vecinos)

            for i in range(len(self.rec_from)):
                if self.rec_from[i] == None:
                    self.rec_from[i] = set()
            for recibido in self.canal_entrada.items:
                for i in range(len(self.rec_from)):
                    if i == recibido[0]:
                        self.rec_from[i] = recibido[1]
            
            self.New = set()
            
            for j in range(len(self.rec_from)):
                if j != self.id_nodo:
                    if self.rec_from[j] != set():
                        for k in self.rec_from[j]:
                            if self.V[k] == None:
                                self.V[k] = k
                                self.New = self.New | {k}
            
        for x in self.V:
            if x != None:
                self.lider = x
                break