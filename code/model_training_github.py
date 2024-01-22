import numpy as np
import tensorflow as tf
import tensorflow_addons as tfa
import keras
import segmentation_models as sm
from keras.metrics import MeanIoU
import matplotlib.pyplot as plt

sm.set_framework('tf.keras')
sm.framework()

# set up dir 
dir_ = "YOUR_DIRECTORY/"

## Set hyperparameter and variables
i = "effnetb4_focc_jacc_11" # MC name
m = "effnetb4" # name backbone
batchsize = 8
prefetch = 1
n_classes = 9
n_epoch = 120
lr = 0.0001
decay = 0.000001
metrics = [tf.keras.metrics.CategoricalAccuracy(name="cat_accuracy"),
           sm.metrics.IOUScore(threshold=0.5, class_indexes=[1,2,3,4,5,6,7,8]),
           sm.metrics.FScore(threshold=0.5, class_indexes=[1,2,3,4,5,6,7,8]),
           tf.keras.metrics.MeanIoU(num_classes=n_classes)]


## Load data CV1
CV1_train_imgs = np.load(dir_ +"Data_sets/image_segmentation/CV2_train_imgs_all_patch.npy")
CV1_train_mask = np.load(dir_+ "Data_sets/image_segmentation/CV2_train_mask_all_patch.npy")
CV1_test_imgs = np.load(dir_+ "Data_sets/image_segmentation/CV2_test_imgs_all_patch.npy")
CV1_test_mask = np.load(dir_ + "Data_sets/image_segmentation/CV2_test_mask_all_patch.npy")


## Data preprocessing
def preprocess_images(image, label):
    label = tf.cast(label, tf.float16)
    image = tf.cast(image, tf.uint8)  
    image = tf.keras.applications.efficientnet.preprocess_input(image)
    image = tf.clip_by_value(image, 0, 255)  
    return(image, label)


with tf.device('/cpu:0'):
    train_data = tf.data.Dataset.from_tensor_slices((CV1_train_imgs, CV1_train_mask))
    train_data = train_data.batch(batchsize, drop_remainder=False)
    train_data = train_data.map(preprocess_images, num_parallel_calls=8)  
    
    test_data = tf.data.Dataset.from_tensor_slices((CV1_test_imgs, CV1_test_mask))
    test_data = test_data.batch(batchsize, drop_remainder=False)
    test_data = test_data.map(preprocess_images, num_parallel_calls=8)
    
with tf.device('/cpu:0'):   
    ## Define loss    
    dice_loss = sm.losses.DiceLoss(class_indexes=[1,2,3,4,5,6,7,8]) 
    focal_loss = sm.losses.CategoricalFocalLoss(class_indexes=[1,2,3,4,5,6,7,8])
    jaccard_loss = sm.losses.JaccardLoss(class_indexes=[1,2,3,4,5,6,7,8])
    categorical_crossentropy = sm.losses.CategoricalCELoss(class_indexes=[1,2,3,4,5,6,7,8])
    
    categorical_focal_jaccard_loss = focal_loss + jaccard_loss
    focal_dice_loss = dice_loss + (1* focal_loss)
    
    
with tf.device('/cpu:0'):  
    ## Load U-net backbone
    backbone_model = sm.Unet("efficientnetb4", classes=9, encoder_weights="imagenet", activation="softmax")
    # or if pregenerated
    #backbone_model = keras.models.load_model(dir_ + "u_net_models/model_u_%s_image_net_weights.hdf5" %m)
    
    
    ## Define optimizer
    #optimizer = tf.keras.optimizers.Adam(learning_rate=lr)
    optimizer = tfa.optimizers.AdamW(learning_rate=lr, weight_decay=decay)
    
    
    ## Compile model
    backbone_model.compile(optimizer=optimizer,
                            loss=categorical_focal_jaccard_loss, 
                            metrics=metrics)
    
    ## Callbacks
    mc = [tf.keras.callbacks.ModelCheckpoint(filepath=dir_ + "Data_sets/image_segmentation/u_net_models_mc/u_net_%s" %i,
                                             save_best_only=True, monitor='val_iou_score', mode='max')]
    #es = [tf.keras.callbacks.EarlyStopping(monitor='val_iou_score', mode='max', patience=20)]

## Train model
with tf.device('/gpu:0'):
    hist = backbone_model.fit(train_data,
                              validation_data=test_data,
                              batch_size=batchsize,
                              epochs=n_epoch,
                              callbacks=[mc],
                              verbose = 0)
    
    
## Model evaluation
with tf.device('/cpu:0'): 
    colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
    
    def plot_metrics(history):
        metrics = ['loss', 'cat_accuracy', 'iou_score', 'f1-score']
        for n, metric in enumerate(metrics):
            name = metric.replace("_"," ").capitalize()
            plt.subplot(2,2,n+1)
            plt.plot(history.epoch, history.history[metric], color=colors[0], label='Train')
            plt.plot(history.epoch, history.history['val_'+metric],
                 color=colors[0], linestyle="--", label='Val')
            plt.xlabel('Epoch')
            plt.ylabel(name)
            if metric == 'loss':
                plt.ylim([0, plt.ylim()[1]])
            elif metric == 'auc':
                plt.ylim([0.5,1])
            else:
                plt.ylim([0,1])
                
            plt.legend();
            
            
    plot_metrics(hist)
    plt.savefig(dir_ + "Data_sets/image_segmentation/performance_plots/u_net_%s" %i)       
      
    
    backbone_model.compile(optimizer=optimizer,
                           loss=categorical_focal_jaccard_loss,
                           metrics=metrics)
    backbone_model.load_weights(dir_ + "Data_sets/image_segmentation/u_net_models_mc/u_net_%s" %i)
            
       
    print(backbone_model.evaluate(test_data, verbose=0))
    


