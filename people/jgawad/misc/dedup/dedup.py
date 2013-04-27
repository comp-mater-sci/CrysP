import os
import sys

class Dedup(object):
    def __init__(self):
        self.sha1_map = {}
        self.file2md5_map = {}

        self.sha1_duplicate_set = set()
        self.dryrun = True

    def load_hashes(self,fname_md5,fname_sha1):
        try:
            with open(fname_md5,'r') as inp_md5, open(fname_sha1,'r') as inp_sha1:
                md5_data = inp_md5.readlines()
                sha1_data = inp_sha1.readlines()
            # Make forward map: sha1 => list of filenames        
            for line in sha1_data:
                file_hash, file_name = line.strip().split(' *')
                self.add_sha1file(file_hash,file_name)
                

            # Make reverse map: filename => md5
            for line in md5_data:
                file_hash, file_name = line.strip().split(' *')
                self.file2md5_map[file_name] = file_hash

            self.verifyDuplicates()
        except IOError as e:
            print('Cannot open file')
            raise e


        
    def add_sha1file(self, key, fname):
        if key in self.sha1_map:
            self.sha1_map[key].append(fname)
            self.sha1_duplicate_set.add(key)
        else:
            self.sha1_map[key] = [ fname ]


    def verifyDuplicates(self):
        # Take all duplicates. For every duplicate, there is a list of alternative filenames.
        # All these names should map to the SAME md5 hash.
        exclusions = set()
        for dupsha1 in self.sha1_duplicate_set:
            all_matched = True
            duplist = self.sha1_map[dupsha1]
            mdlist = []
            for fname in duplist:
                try:
                    mdlist.append(self.file2md5_map[fname])
                except:
                    break
                    
            # It is sufficient that 
            if ((len(mdlist) == 0) or (len(mdlist) != len(duplist)) 
                or not all((mdhash == mdlist[0] for mdhash in mdlist))):
                exclusions.add(dupsha1)
        # Extract exclusions from the set
        self.sha1_duplicate_set.difference_update(exclusions)
        pass
            
        
    def print_duplicates(self):
        print '# of duplicates: ' + str(len(self.sha1_duplicate_set))
        for dup in self.sha1_duplicate_set:
            print ('------ hash=' + dup + ' -------')
            print ('# of aliases: '+ str(len(self.sha1_map[dup])))
            for val in self.sha1_map[dup]:
                print val
            
            print ('-------------------------------')


    def resolve_duplicates(self,dup_hash):
        # Operate on a copy of the list!!
        duplist = self.sha1_map[dup_hash][:]
        chosen = duplist[0]
        chosen_fname = os.path.basename(chosen)
        for fname in duplist[1:]:
            test_fname = os.path.basename(fname)
            # print(test_fname)
            if  len(test_fname) > len(chosen_fname):
                chosen = fname
                chosen_fname = test_fname
        duplist.remove(chosen)
        return (chosen, duplist)
        
    def proposeSolution(self):
        for dup_hash in self.sha1_duplicate_set:
            fulllist = self.sha1_map[dup_hash]
            chosen,removelist = self.resolve_duplicates(dup_hash)
            #print 'List of duplicates:'
            #for fname in fulllist:
            #    print(fname)
            print('To be preserved: ' + chosen)
            print('To be removed ('+ str(len(removelist)) + ' item(s)):')
            for fname in removelist:
                print(fname)
            if not self.dryrun:
                # Propose removal
                sys.stdin.flush()
                print('Proceed? [Y|n]')
                l = sys.stdin.read(1)
                if (l in ['\n','y','Y']):
                    # let's do it...
                    for fname in removelist:
                        try:
                            if os.path.exists(fname):
                                print ('Removing ' + fname)
                                os.unlink(fname)
                            else:
                                print('File not found: ' + fname)
                        except OSError as e:
                            print e
                else:
                    print('Skipping...')

            print('------------------')
            print('')
                

if __name__ == "__main__":
    dd = Dedup()
    #dd.load_hashes('sample_md5sum.txt','sample_sha1sum.txt')
    
    dd.load_hashes('pdflist_md5sum.txt','pdflist_sha1sum.txt')
    #dd.print_duplicates()
    dd.dryrun = False
    dd.proposeSolution()
    pass
