from  Cheetah.Filters import *

class WidthFilter(Filter):
    def filter(self, val, **kw):
        fmt = '{val:<20}'
        if 'width' in kw:
            # make fmt string
            fmt = '{{val:<{width}}}'.format(**kw)
        return fmt.format(val=str(val))
        
class TensorFilter(Filter):
    def filter(self, val, **kw):
        n = 3
        return '\n'.join([' '.join((str(float(item)) for item in val[i:i+n])) for i in xrange(0, len(val), n)])

class PlainIterableFilter(Filter):
    def filter(self, val, **kw):
        
        fmt = kw.get('fmt', '{}')
        return ' '.join((fmt.format(item) for item in val))
        
        
class FilterChain(Filter):
    keyword_filters = {'asTensor': TensorFilter(),
                       'plain': PlainIterableFilter(),
                       'none': Filter()}
    filters = [WidthFilter()]
    def filter(self, val, **kw):
        # Filtering chain: keyword-activated filters first
        # We iterate over items in kw, because it is likely
        # shorter.
        for key in kw.keys():
            if key in self.keyword_filters:
                val = self.keyword_filters[key].filter(val, **kw)
        
        if kw.get('bypass',None):
            return str(val)
        # and whatever comes out, pass it through the 
        # output filters
        for f in self.filters:
            val = f.filter(val, **kw)
        return str(val)
