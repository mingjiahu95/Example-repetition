% output a frequency table of block number of old items
tokens = token_store(phase_store==2 & itemtype_store==1);
cats = cat_store(phase_store==2 & itemtype_store==1);


blocks=[];
for i = 1:length(tokens)
    icat = cats(i);itoken = tokens(i);
    TFvec = phase_store==1 & cat_store == icat & token_store == itoken;
    if ~TFvec
        blocks(i) = 0;
    else
        blocks(i) = block_store(TFvec);
    end
end


tabulate(blocks)
        