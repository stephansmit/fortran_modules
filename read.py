import f90nml
patch_nml = {'input': {'eosmode': 1}}
f90nml.patch('input.nml', patch_nml, 'new_sample.nml')


#nml = f90nml.read('input.nml')
#nml['input']['label']='test'
#print(nml)
